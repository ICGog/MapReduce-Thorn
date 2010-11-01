-module(smr_master).
-behavior(gen_server).

-export([start_link/0, spawn_worker/2, shutdown_worker/2, print_workers/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, 
        code_change/3]).

-record(state, {workers = dict:new()}).

-record(worker, {node_name,
                 node_pid,
                 num_executing = 0,
                 num_failed = 0,
                 num_succ = 0}).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

start_link() -> gen_server:start_link(?MODULE, [], []).

spawn_worker(Pid, Node) -> 
    gen_server:call(Pid, {spawn_worker, Node}, infinity).

shutdown_worker(Pid, Node) -> 
    gen_server:call(Pid, {shutdown_worker, Node}, infinity).

print_workers(Pid) -> gen_server:call(Pid, print_workers, infinity).

%------------------------------------------------------------------------------
% Handlers
%------------------------------------------------------------------------------

init([]) -> {ok, #state{}}.

handle_call({spawn_worker, WorkerNode}, _From,
            State = #state{workers = Workers}) ->
    case dict:find(WorkerNode, Workers) of
        {ok, _Val} ->
             {reply, {error, already_registered}, State};
        error ->
             WorkerPid = proc_lib:spawn_link(
                 WorkerNode,
                 fun () ->
                         {ok, WorkerState} = smr_worker:init([]),
                         gen_server:enter_loop(smr_worker, [], WorkerState,
                                               infinity)
                 end),
             NewWorkers = dict:store(
                 WorkerNode,
                 #worker{node_name = WorkerNode,
                         node_pid = WorkerPid},
                 Workers),
             {reply, ok, State#state{workers = NewWorkers}}
    end;

handle_call({shutdown_worker, WorkerNode}, _From,
            State = #state{workers = Workers}) ->
    case dict:find(WorkerNode, Workers) of
        {ok, _Val} -> {reply, ok,
                       State#state{workers = dict:erase(WorkerNode, Workers)}};
        error      -> {reply, {error, not_registered}, State}
    end;

handle_call(print_workers, _From, State) ->
    {reply, dict:fetch_keys(State#state.workers), State}.

handle_cast(_Request, State) -> {stop, unexpected_cast, State}.

handle_info(Message, State) -> {stop, unexpected_message, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

