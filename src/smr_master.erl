
-module(smr_master).

-behavior(gen_server).

-export([start_link/0, spawn_worker/2, shutdown_worker/2, get_worker_nodes/1,
         get_worker_pids/1, map_reduce/4]).
-export([job_result/3, allocate_workers/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, 
        code_change/3]).

-record(state, {workers = dict:new(), jobs = dict:new(), 
                workers_pid = dict:new()}).

-record(worker, {node,
                 pid,
                 num_executing = 0,
                 num_failed = 0,
                 num_succ = 0}).

-record(job, {pid, from, map_fun, reduce_fun, input}).

-define(MIN_BACKOFF_MS, 64).
-define(MAX_BACKOFF_MS, 32768).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

spawn_worker(Pid, Node) -> 
    gen_server:call(Pid, {spawn_worker, Node}, infinity).

shutdown_worker(Pid, Node) -> 
    gen_server:call(Pid, {shutdown_worker, Node}, infinity).

get_worker_nodes(Pid) -> gen_server:call(Pid, get_worker_nodes, infinity).

get_worker_pids(Pid) -> gen_server:call(Pid, get_worker_pids, infinity).

map_reduce(Pid, Map, Reduce, Input) -> 
    gen_server:call(Pid, {map_reduce, Map, Reduce, Input}, infinity).

%------------------------------------------------------------------------------
% Internal API
%------------------------------------------------------------------------------

job_result(Pid, JobPid, Result) ->
    gen_server:cast(Pid, {job_result, JobPid, Result}).

allocate_workers(Pid) ->
    gen_server:call(Pid, allocate_workers, infinity).

%------------------------------------------------------------------------------
% Handlers
%------------------------------------------------------------------------------

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({spawn_worker, WorkerNode}, _From,
            State = #state{workers = Workers, workers_pid = WorkersPid}) ->
    case dict:find(WorkerNode, Workers) of
        {ok, _Val} ->
             {reply, {error, already_registered}, State};
        error ->
             {ok, WorkerPid} = smr_worker:start_link(WorkerNode),
             NewWorkers = dict:store(WorkerNode, #worker{node = WorkerNode,
                                                         pid = WorkerPid},
                                     Workers),
             {reply, ok, State#state{workers = NewWorkers,
                                     workers_pid = dict:store(WorkerPid,
                                                              WorkerNode,
                                                              WorkersPid)}}
    end;

handle_call({shutdown_worker, WorkerNode}, _From,
            State = #state{workers = Workers, workers_pid = WorkersPid}) ->
    case dict:find(WorkerNode, Workers) of
        {ok, Val} -> {reply, ok,
                      State#state{workers = dict:erase(WorkerNode, Workers),
                                  workers_pid = dict:erase(Val#worker.pid, 
                                                           WorkersPid)}};
        error      -> {reply, {error, not_registered}, State}
    end;

handle_call(get_worker_nodes, _From, State) ->
    {reply, dict:fetch_keys(State#state.workers), State};

handle_call(get_worker_pids, _From, State) ->
    {reply, dict:fetch_keys(State#state.workers_pid), State};

handle_call(allocate_workers, _From, State = #state{workers = Workers}) ->
    {reply, dict:fold(fun (_, #worker{pid = Pid}, Acc) -> [Pid | Acc] end,
                      [], Workers),
     State};

handle_call({map_reduce, Map, Reduce, Input}, From, 
            State = #state{jobs = Jobs}) ->
    {ok, JobPid} = smr_job:start_link(self(), Map, Reduce, Input),
    {noreply, State#state{jobs = dict:store(JobPid, #job{pid = JobPid,
                                                         from = From,
                                                         map_fun = Map,
                                                         reduce_fun = Reduce,
                                                         input = Input},
                                            Jobs)}}.

handle_cast({job_result, JobPid, Result}, State = #state{jobs = Jobs}) ->
    gen_server:reply((dict:fetch(JobPid, Jobs))#job.from, Result),
    {noreply, State}.

handle_info({'EXIT', JobPid, normal}, State = #state{jobs = Jobs}) ->
    error_logger:info_msg("MapReduce job ~p ended~n", [JobPid]), 
    {noreply, State#state{jobs = dict:erase(JobPid, Jobs)}};

% Catches a worker's exit and tries to restart it using exponential backoff.
% Carefull, this catches all the EXIT messages.
handle_info({'EXIT', WorkerPid, _Reason},
            State = #state{workers = Workers, workers_pid = WorkersPid}) ->
    error_logger:error_msg("Worker ~p crashed~n", [dict:fetch(WorkerPid, 
                                                              WorkersPid)]),
    NewWorkersPid = dict:erase(WorkerPid, WorkersPid),
    erlang:send_after(?MIN_BACKOFF_MS, self(), 
                      {restart_worker, dict:fetch(
                                            dict:fetch(WorkerPid, WorkersPid), 
                                            Workers), 
                       ?MIN_BACKOFF_MS}),
    {noreply, State#state{workers_pid = NewWorkersPid}};

handle_info({restart_worker, Worker, Backoff},
            State = #state{workers = Workers}) ->
    case Backoff =< ?MAX_BACKOFF_MS of
        true  -> {noreply, restart_worker(
                    smr_worker:start_link(Worker#worker.node), 
                    Worker, Backoff, State)};
        false -> {noreply, State#state{workers = dict:erase(Worker#worker.node, 
                                                       Workers)}}
                  % TODO: Update the statistics
    end;
    
handle_info(Msg, State) ->
    {stop, {unexpected_message, Msg}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%------------------------------------------------------------------------------
% Internal
%------------------------------------------------------------------------------

restart_worker({ok, WorkerPid}, Worker, _Backoff,
        State = #state{workers = Workers, workers_pid = WorkersPid}) ->
    error_logger:info_msg("Worker ~p restarted~n", [Worker#worker.node]),
    State#state{workers = dict:store(Worker#worker.node, 
                                     Worker#worker{pid = WorkerPid},
                                     Workers), 
                workers_pid = dict:store(WorkerPid, Worker#worker.node,
                                         WorkersPid)};

restart_worker({error, _Reason}, Worker, Backoff, State) ->
    NewBackoff = Backoff * 2,
    erlang:send_after(NewBackoff, self(), {restart_worker, Worker, NewBackoff}),
    State.

