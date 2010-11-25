-module(smr_statistics).

-behaviour(gen_server).

-export([start_link/0, register_worker/2, unregister_worker/2,
        worker_failed/2, worker_batch_started/6, worker_batch_ended/5,
        worker_batch_failed/3]).

-export([get_all_workers/1]).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2,
        code_change/3]).

-record(state, {workers = dict:new()}).

-record(worker, {node, 
                 num_exec = 0,
                 num_failed = 0,
                 num_succ = 0,
                 num_jobs = 0,
                 busy_time = 0,
                 num_map_jobs = 0,
                 num_reduce_jobs = 0,
                 start_time = dict:new()}).

%------------------------------------------------------------------------------
% Internal API
%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({global, smr_statistics}, ?MODULE, [], []).

register_worker(Pid, Node) ->
    gen_server:cast(Pid, {register_worker, Node}).

unregister_worker(Pid, Node) ->
    gen_server:cast(Pid, {unregister_worker, Node}).

worker_failed(Pid, Node) ->
    gen_server:cast(Pid, {worker_failed, Node}).

worker_batch_started(Pid, Node, JobType, JobId, Time, BatchSize) ->
    gen_server:cast(Pid, {batch_started, Node, JobType, JobId, Time, 
                          BatchSize}).

worker_batch_failed(Pid, Node, BatchSize) ->
    gen_server:cast(Pid, {batch_failed, Node, BatchSize}).

worker_batch_ended(Pid, Node, JobId, Time, BatchSize) ->
    gen_server:cast(Pid, {batch_ended, Node, JobId, Time, BatchSize}).

%------------------------------------------------------------------------------
% Query API
%------------------------------------------------------------------------------

get_all_workers(Pid) ->
    gen_server:call(Pid, get_all_workers).

%------------------------------------------------------------------------------
% Handlers
%------------------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call(get_all_workers, _From, State = #state{workers = Workers}) ->
    {reply, dict:fetch_keys(Workers), State}.

handle_cast({register_worker, Node},
            State = #state{workers = Workers}) ->
    {noreply, State#state{workers = dict:store(Node,
                                               #worker{node = Node},
                                               Workers)}};

handle_cast({unregister_worker, Node},
            State = #state{workers = Workers}) ->
    {noreply, State#state{workers = dict:erase(Node, Workers)}};

handle_cast({worker_failed, Node},
            State = #state{workers = Workers}) ->
    Worker = #worker{num_failed = Failed,
                     num_exec = Exec} = dict:fetch(Node, Workers),
    {noreply, State#state{
                workers = dict:store(Node, 
                                     Worker#worker{num_failed = Failed + Exec,
                                                   num_exec = 0}, 
                                     Workers)}};

handle_cast({batch_started, Node, JobType, JobId, Time, Size},
            State = #state{workers = Workers}) ->
    Worker = #worker{num_exec = Exec,
                     num_map_jobs = NumMap,
                     num_reduce_jobs = NumReduce,
                     start_time = StartTime} = dict:fetch(Node, Workers),
    Worker1 = Worker#worker{num_exec = Exec + Size,
                            start_time = dict:store(JobId, Time, StartTime)},
    NewWorker = case JobType of
                    map    -> Worker1#worker{num_map_jobs = NumMap};
                    reduce -> Worker1#worker{num_reduce_jobs = NumReduce}
                end,
    {noreply, State#state{workers = dict:store(Node, NewWorker, Workers)}};

handle_cast({batch_ended, Node, JobId, Time, Size},
            State = #state{workers = Workers}) ->
    Worker = #worker{num_exec = Exec,
                     num_succ = Succ,
                     start_time = STime,
                     busy_time = BTime} = dict:fetch(Node, Workers),
    NewBTime = BTime + Time - dict:fetch(JobId, STime),
    {noreply, State#state{workers = 
                 dict:store(Node,
                            Worker#worker{num_exec = Exec - Size,
                                          num_succ = Succ + Size,
                                          busy_time = NewBTime,
                                          start_time = dict:erase(JobId, 
                                                                  STime)},
                            Workers)}};

handle_cast({batch_failed, Node, Size},
            State = #state{workers = Workers}) ->
    Worker = #worker{num_failed = Failed,
                     num_exec = Exec} = dict:fetch(Node, Workers),
    {noreply, State#state{workers = dict:store(Node,
                                               Worker#worker{
                                                  num_exec = Exec - Size,
                                                  num_failed = Failed + Size},
                                               Workers)}};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    {stop, {unexpected_message, Msg}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
