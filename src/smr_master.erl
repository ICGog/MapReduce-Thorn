-module(smr_master).
-behavior(gen_server).

-export([start_link/0, register_worker/2, unregister_worker/2, 
         print_workers/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, 
        code_change/3]).

-record(state, {workers :: gb_tree()}).

-record(worker_node, {node_name :: nonempty_string(),
                      node_pid :: pid(),
                      num_executing :: non_neg_integer(),
                      num_failed :: non_neg_integer(),
                      num_succ :: non_neg_integer()}).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

start_link() -> gen_server:start_link(?MODULE, [], []).

register_worker(Pid, Worker) -> 
    gen_server:call(Pid, {register_worker, Worker}, infinity).

unregister_worker(Pid, Worker) -> 
    gen_server:call(Pid, {unregister_worker, Worker}, infinity).

print_workers(Pid) -> gen_server:call(Pid, print_workers, infinity).

%------------------------------------------------------------------------------
% Handlers
%------------------------------------------------------------------------------

init([]) -> {ok, #state{workers = gb_trees:empty()}}.

handle_call({register_worker, WorkerNode}, _From, State) ->
    case gb_trees:lookup(WorkerNode, State#state.workers) of
        {value, _Val} -> {reply, worker_already_registered, State};
        none          -> StartWorker = fun() ->
                             {ok, WorkerState} = smr_worker:init([]),
                             gen_server:enter_loop(
                                                   smr_worker, 
                                                   [], 
                                                   WorkerState, 
                                                   infinity)
                         end,
                         WorkerPid = proc_lib:spawn_link(
                                                         WorkerNode, 
                                                         StartWorker 
                                                        ),
                         NewNode = #worker_node{node_name = WorkerNode,
                                                node_pid = WorkerPid,
                                                num_executing = 0,
                                                num_failed = 0,
                                                num_succ = 0},
                         NewState = State#state{
                             workers = gb_trees:insert(
                                                       WorkerNode, 
                                                       NewNode, 
                                                       State#state.workers)},
                         {reply, worker_registered, NewState}
    end;

handle_call({unregister_worker, Worker}, _From, State) ->
    case gb_trees:lookup(Worker, State#state.workers) of
        {value, _Val} -> {reply, 
                          worker_unregistered, 
                          State#state{workers = gb_trees:delete(
                                                    Worker, 
                                                    State#state.workers)}};
        none          -> {reply, worker_not_registered, State}
    end;

handle_call(print_workers, _From, State) ->
    {reply, gb_trees:keys(State#state.workers), State}.

handle_cast(_Request, _State) -> {}.

handle_info(Message, State) ->
    error_logger:warning_msg("Unknown message received: ~p~n", [Message]),
    {stop, unexpected_message, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.
