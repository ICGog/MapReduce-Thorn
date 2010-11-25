
-module(smr_master).

-behavior(gen_server).

-export([start_link/0, spawn_worker/2, shutdown_worker/2, get_worker_nodes/1,
         get_worker_pids/1, map_reduce/4]).
-export([job_result/3, allocate_workers/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, 
        code_change/3]).

-record(state, {sup,
                workers = dict:new(),
                jobs = dict:new(),
                workers_pid = dict:new()}).

-record(worker, {node, pid}).

-record(job, {pid, from, map_fun, reduce_fun, input}).

-define(MIN_BACKOFF_MS, 64).
-define(MAX_BACKOFF_MS, 32768).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({global, smr_master}, ?MODULE, [self()], []).

spawn_worker(Pid, Node) ->
    gen_server:call(Pid, {spawn_worker, Node}, infinity).

shutdown_worker(Pid, Node) ->
    gen_server:call(Pid, {shutdown_worker, Node}, infinity).

get_worker_nodes(Pid) ->
    gen_server:call(Pid, get_worker_nodes, infinity).

get_worker_pids(Pid) ->
    gen_server:call(Pid, get_worker_pids, infinity).

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

init([Sup]) ->
    {ok, #state{sup = Sup}}.

handle_call({spawn_worker, Node}, _From,
            State = #state{workers = Workers, sup = Sup}) ->
    case dict:find(Node, Workers) of
        {ok, _Val} -> {reply, {error, already_registered}, State};
        error      -> Stats = smr_sup:statistics(Sup),
                      smr_statistics:register_worker(Stats, Node),
                      {reply, ok, restart_worker(Node, State)}
    end;

handle_call({shutdown_worker, Node}, _From,
            State = #state{workers = Workers}) ->
    case dict:find(Node, Workers) of
        {ok, #worker{pid = Pid}} -> smr_worker:shutdown(Pid),
                                    {reply, ok, State};
        error ->                    {reply, {error, not_registered}, State}
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
            State = #state{sup = Sup, jobs = Jobs}) ->
    {ok, JobPid} = smr_job_sup:start_job(smr_sup:job_sup(Sup), self(),
                                         Map, Reduce, Input),
    {noreply, State#state{jobs = dict:store(JobPid, #job{pid = JobPid,
                                                         from = From,
                                                         map_fun = Map,
                                                         reduce_fun = Reduce,
                                                         input = Input},
                                            Jobs)}}.

handle_cast({job_result, JobPid, Result}, State = #state{jobs = Jobs}) ->
    error_logger:info_msg("MapReduce job ~p successfully completed~n",
                          [JobPid]),
    gen_server:reply((dict:fetch(JobPid, Jobs))#job.from, {ok, Result}),
    {noreply, State}.

handle_info({restart_worker, Node, Backoff},
            State = #state{workers = Workers}) ->
    case Backoff =< ?MAX_BACKOFF_MS of
        true  -> {noreply, restart_worker(Node, 2 * Backoff, State)};
        false -> error_logger:info_msg("Reached maximum restart backoff time "
                                       "for worker ~p~n", [Node]),
                 {noreply, State#state{workers = dict:erase(Node, Workers)}}
    end;

handle_info({'DOWN', _, process, Pid, Reason},
            State = #state{workers_pid = WorkersPid, jobs = Jobs}) ->
    case {dict:find(Pid, WorkersPid), dict:is_key(Pid, Jobs)} of
        {{ok, WorkerNode}, false} -> handle_worker_exit(Pid, WorkerNode, Reason,
                                                        State);
        {error, true}             -> handle_job_exit(Pid, Reason, State)
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

handle_worker_exit(Pid, Node, normal,
                   State = #state{sup = Sup,
                                  workers = Workers,
                                  workers_pid = WorkersPid}) ->
    error_logger:info_msg("Worker ~p shutdown successfully~n", [Node]),
    smr_statistics:unregister_worker(smr_sup:statistics(Sup), Node),
    {noreply, State#state{workers = dict:erase(Node, Workers),
                          workers_pid = dict:erase(Pid, WorkersPid)}};
handle_worker_exit(Pid, Node, Reason,
                   State = #state{workers_pid = WorkersPid}) ->
    error_logger:info_msg("Worker ~p crashed. Reason: ~p~n", [Node, Reason]),
    {noreply,
     restart_worker(Node,
                    State#state{workers_pid = dict:erase(Pid, WorkersPid)})}.

handle_job_exit(Pid, Reason, State = #state{jobs = Jobs}) ->
    case Reason of
        normal ->
            ok;
        _ ->
            error_logger:info_msg("MapReduce job ~p failed. Reason: ~p~n",
                                  [Pid, Reason]),
            gen_server:reply((dict:fetch(id, Jobs))#job.from, {error, Reason})
    end,
    {noreply, State#state{jobs = dict:erase(Pid, Jobs)}}.

restart_worker(Node, State) ->
    restart_worker(Node, ?MIN_BACKOFF_MS, State).

restart_worker(Node, BackOff, State = #state{sup = Sup,
                                             workers = Workers,
                                             workers_pid = WorkersPid}) ->
    error_logger:info_msg("(Re)starting worker ~p~n", [Node]),
    case smr_worker_sup:start_worker(smr_sup:worker_sup(Sup), Node) of
        {ok, Pid} ->
            error_logger:info_msg("Worker ~p started~n", [Node]),
            erlang:monitor(process, Pid),
            Worker = case dict:find(Node, Workers) of
                         {ok, W} -> W;
                         error   -> #worker{pid = Pid}
                     end,
            State#state{workers = dict:store(Node, Worker#worker{node = Node},
                                             Workers),
                        workers_pid = dict:store(Pid, Node, WorkersPid)};
        _Error ->
            error_logger:info_msg("Waiting for ~p before restarting worker "
                                  "~p~n", [BackOff, Node]),
            erlang:send_after(BackOff, self(), {restart_worker, Node, BackOff}),
            State
    end.
