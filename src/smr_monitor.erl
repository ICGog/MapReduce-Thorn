-module(smr_monitor).

-behaviour(gen_server).

-export([start_link/0, register_worker/2, unregister_worker/2,
        worker_failed/2, worker_batch_started/6, worker_batch_ended/5,
        worker_batch_failed/3]).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2,
        code_change/3]).

-record(state, {workers = dict:new_dict()}).

-record(worker, {node, num_executing = 0, num_failed = 0, num_succ = 0,
                num_jobs = 0, busy_time = 0, num_map_jobs = 0, 
                num_reduce_jobs = 0, start_time = dict:new_dict()}).

%------------------------------------------------------------------------------
% Internal API
%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

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
% Handlers
%------------------------------------------------------------------------------

init([]) ->
    {ok, none}.

handle_call(Request, _From, State) ->
    {stop, {unexpected_call, Request}, State}.

handle_cast({register_worker, WorkerNode},
            State = #state{workers = Workers}) ->
    {noreply, State#state{workers = dict:store(WorkerNode,
                                               #worker{node = WorkerNode},
                                               Workers)}};

handle_cast({unregister_worker, WorkerNode},
            State = #state{workers = Workers}) ->
    {noreply, State#state{workers = dict:erase(WorkerNode, Workers)}};

handle_cast({worker_failed, WorkerNode},
            State = #state{workers = Workers}) ->
    Worker = dict:fetch(WorkerNode, Workers),
    NumFailed = Worker#worker.num_failed + Worker#worker.num_executing,
    {noreply, State#state{
                workers = dict:store(WorkerNode, 
                                     Worker#worker{num_failed = NumFailed,
                                                   num_executing = 0}, 
                                     Workers)}};

handle_cast({batch_started, WorkerNode, JobType, JobId, Time, BatchSize},
            State = #state{workers = Workers}) ->
    Worker = dict:fetch(WorkerNode, Workers),
    NumExecuting = Worker#worker.num_executing + BatchSize,
    StartTime = dict:store(JobId, Time, Worker#worker.start_time),
    if
        JobType == map    -> NumMapJobs = Worker#worker.num_map_jobs,
                             NewWorker = Worker#worker{
                                 num_executing = NumExecuting, 
                                 num_map_jobs = NumMapJobs,
                                 start_time = StartTime};
        JobType == reduce -> NumReduceJobs = Worker#worker.num_reduce_jobs,
                             NewWorker = Worker#worker{
                                 num_executing = NumExecuting,
                                 num_reduce_jobs = NumReduceJobs,
                                 start_time = StartTime}
    end,
    {noreply, State#state{workers = dict:store(WorkerNode,
                                               NewWorker,
                                               Workers)}};

handle_cast({batch_ended, WorkerNode, JobId, Time, BatchSize},
            State = #state{workers = Workers}) ->
    Worker = dict:fetch(WorkerNode, Workers),
    NumExecuting = Worker#worker.num_executing - BatchSize,
    NumSucc = Worker#worker.num_succ + BatchSize,
    StartTime = Worker#worker.start_time,
    BusyTime = Worker#worker.busy_time + Time - dict:fetch(JobId, StartTime),
    {noreply, State#state{workers = 
                 dict:store(WorkerNode,
                            Worker#worker{num_executing = NumExecuting,
                                          num_succ = NumSucc,
                                          busy_time = BusyTime,
                                          start_time = dict:erase(JobId, 
                                                                  StartTime)},
                            Workers)}};

handle_cast({batch_failed, WorkerNode, BatchSize},
            State = #state{workers = Workers}) ->
    Worker = dict:fetch(WorkerNode, Workers),
    NumFailed = Worker#worker.num_failed + BatchSize,
    NumExecuting = Worker#worker.num_executing - BatchSize,
    {noreply, State#state{workers = dict:store(WorkerNode,
                                               Worker#worker{
                                                  num_executing = NumExecuting,
                                                  num_failed = NumFailed},
                                               Workers)}};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    {stop, {unexpected_message, Msg}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

