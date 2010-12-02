-module(smr_statistics).

-behaviour(gen_server).

-export([start_link/1, register_worker/2, worker_failed/2,
         worker_batch_started/3, worker_batch_ended/2, worker_batch_failed/2]).

-export([get_all_workers/1, get_workers/1]).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2,
        code_change/3]).

-include("smr.hrl").

-record(state, {workers = dict:new()}).

%------------------------------------------------------------------------------
% Query API
%------------------------------------------------------------------------------

get_all_workers(Pid) ->
    gen_server:call(Pid, get_all_workers).

get_workers(Pid) ->
    gen_server:call(Pid, get_workers).

%------------------------------------------------------------------------------
% Internal API
%------------------------------------------------------------------------------

start_link(Nodes) ->
    gen_server:start_link({global, smr_statistics}, ?MODULE, [Nodes], []).

register_worker(Pid, Node) ->
    gen_server:cast(Pid, {register_worker, Node}).

worker_failed(Pid, Node) ->
    gen_server:cast(Pid, {worker_failed, Node}).

worker_batch_started(Pid, Node, TaskType) ->
    gen_server:cast(Pid, {batch_started, Node, TaskType}).

worker_batch_failed(Pid, Node) ->
    gen_server:cast(Pid, {batch_failed, Node}).

worker_batch_ended(Pid, Node) ->
    gen_server:cast(Pid, {batch_ended, Node}).

%------------------------------------------------------------------------------
% Handlers
%------------------------------------------------------------------------------

init([Nodes]) ->
    {ok, lists:foldl(fun internal_register_worker/2, #state{}, Nodes)}.

handle_call(get_all_workers, _From, State = #state{workers = Workers}) ->
    {reply, dict:fetch_keys(Workers), State};
handle_call(get_workers, _From, State = #state{workers = Workers}) ->
    {reply, dict:to_list(Workers), State}.

handle_cast({register_worker, Node}, State) ->
    {noreply, internal_register_worker(Node, State)};
handle_cast({worker_failed, Node},
            State = #state{workers = Workers}) ->
    Worker = #worker{num_failed = Failed,
                     num_exec = Exec} = dict:fetch(Node, Workers),
    % TODO: Find a way of updating busy_time.
    {noreply, State#state{
                workers = dict:store(Node, 
                                     Worker#worker{num_failed = Failed + Exec,
                                                   num_exec = 0},
                                     Workers)}};
handle_cast({batch_started, Node, TaskType},
            State = #state{workers = Workers}) ->
    Worker = #worker{num_exec = Exec,
                     num_map_tasks = NumMap,
                     num_reduce_tasks = NumReduce} = dict:fetch(Node, Workers),
    Worker1 = Worker#worker{num_exec = Exec + 1},
    NewWorker = case TaskType of
                    map    -> Worker1#worker{num_map_tasks = NumMap + 1};
                    reduce -> Worker1#worker{num_reduce_tasks = NumReduce + 1}
                end,
    {noreply, State#state{workers = dict:store(Node, NewWorker, Workers)}};
handle_cast({batch_ended, Node},
            State = #state{workers = Workers}) ->
    Worker = #worker{num_exec = Exec,
                     num_succ = Succ,
                     busy_time = BTime} = dict:fetch(Node, Workers),
    % TODO: Find a way of correctly computing busy_time.
    {noreply, State#state{workers = 
                 dict:store(Node,
                            Worker#worker{num_exec = Exec - 1,
                                          num_succ = Succ + 1},
                            Workers)}};
handle_cast({batch_failed, Node},
            State = #state{workers = Workers}) ->
    Worker = #worker{num_failed = Failed,
                     num_exec = Exec} = dict:fetch(Node, Workers),
    {noreply, State#state{workers = dict:store(Node,
                                               Worker#worker{
                                                  num_exec = Exec - 1,
                                                  num_failed = Failed + 1},
                                               Workers)}}.

handle_info({nodedown, Node}, State = #state{workers = Workers}) ->
    true = dict:is_key(Node, Workers), %% assertion
    %% TODO: do we want to erase everything about this worker?
    {noreply, State#state{workers = dict:erase(Node, Workers)}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%------------------------------------------------------------------------------
% Internal plumbing
%------------------------------------------------------------------------------

internal_register_worker(Node, State = #state{workers = Workers}) ->
    monitor_node(Node, true),
    State#state{workers = dict:store(Node, #worker{node = Node}, Workers)}.

