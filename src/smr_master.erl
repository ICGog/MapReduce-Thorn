
-module(smr_master).

-behavior(gen_server).

-export([start_link/0, new_job/2, new_job/4, add_input/2, do_job/1]).
-export([job_result/2]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, 
        code_change/3]).

-record(state, {jobs = dict:new(),    %% (JobPid -> #job{})
                cur_job_id = 1,
                job_pids = dict:new()  %% (JobId -> JobPid)
               }).

-record(job, {pid,
              id,
              from,
              map_fun,
              reduce_fun}).

-define(NAME, {global, ?MODULE}).
-define(DEFAULT_BATCH_SIZE, 10).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?NAME, ?MODULE, [], []).

new_job(Map, Reduce) ->
    gen_server:call(?NAME, {new_job, Map, Reduce,
                            ?DEFAULT_BATCH_SIZE, ?DEFAULT_BATCH_SIZE},
                    infinity).

new_job(Map, Reduce, MapBatchSize, ReduceBatchSize) ->
    gen_server:call(?NAME, {new_job, Map, Reduce,
                            MapBatchSize, ReduceBatchSize},
                    infinity).

add_input(JobId, Input) ->
    gen_server:cast(?NAME, {add_input, JobId, Input}).

do_job(JobId) ->
    gen_server:call(?NAME, {do_job, JobId}, infinity).

%------------------------------------------------------------------------------
% Internal API
%------------------------------------------------------------------------------

job_result(JobPid, Result) ->
    gen_server:cast(?NAME, {job_result, JobPid, Result}).

%------------------------------------------------------------------------------
% Handlers
%------------------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call({new_job, Map, Reduce, MapBatchSize, ReduceBatchSize}, _From,
            State = #state{jobs = Jobs,
                           cur_job_id = JobId,
                           job_pids = JobPids}) ->
    {ok, JobSup} = smr_job_sup_sup:start_job_sup(Map, Reduce, JobId,
                                                 MapBatchSize, ReduceBatchSize),
    JobPid = smr_job_sup:job(JobSup),
    erlang:monitor(process, JobPid),
    NewJobPids = dict:store(JobId, JobPid, JobPids),
    NewJobs = dict:store(JobPid, #job{pid = JobPid,
                                      id = JobId,
                                      map_fun = Map,
                                      reduce_fun = Reduce}, Jobs),
    {reply, {ok, JobId}, State#state{jobs = NewJobs,
                                     job_pids = NewJobPids,
                                     cur_job_id = JobId + 1}};
handle_call({do_job, JobId}, From, State = #state{job_pids = JobPids,
                                                  jobs = Jobs}) ->
    JobPid = dict:fetch(JobId, JobPids),
    Job = dict:fetch(JobPid, Jobs),
    smr_job:start(JobPid),
    {noreply,
     State#state{jobs = dict:store(JobPid, Job#job{from = From}, Jobs)}}.

handle_cast({add_input, JobId, Input}, State = #state{job_pids = JobPids}) ->
    smr_job:add_input(dict:fetch(JobId, JobPids), Input),
    {noreply, State};
handle_cast({job_result, JobPid, Result}, State = #state{jobs = Jobs}) ->
    #job{from = From, id = Id} = dict:fetch(JobPid, Jobs),
    error_logger:info_msg("MapReduce job ~p successfully completed~n", [Id]),
    gen_server:reply(From, {ok, Result}),
    {noreply, State}.

handle_info({'DOWN', _, process, Pid, Reason}, State = #state{jobs = Jobs}) ->
    handle_job_exit(Pid, dict:fetch(Pid, Jobs), Reason, State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%------------------------------------------------------------------------------
% Internal
%------------------------------------------------------------------------------

handle_job_exit(Pid, #job{id = Id, from = From}, Reason,
                State = #state{jobs = Jobs, job_pids = JobPids}) ->
    case Reason of
        normal -> ok;
        _      -> error_logger:info_msg("MapReduce job ~p failed. Reason: ~p~n",
                                        [Id, Reason]),
                  smr_statistics:job_failed(Id, Reason),
                  gen_server:reply(From, {error, Reason})
    end,
    {noreply, State#state{jobs     = dict:erase(Pid, Jobs),
                          job_pids = dict:erase(Id, JobPids)}}.
