
-module(smr_master).

-behavior(gen_server).

-export([start_link/0, new_job/2, new_job/3, new_job/4, new_job_from_another/3,
         new_job_from_another/4, new_job_from_another/5, add_input/2,
         import_from_output/2, do_job/1, next_result/1, whole_result/1,
         kill_job/1]).
-export([job_finished/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, 
        code_change/3]).

-record(state, {jobs = dict:new(), %% (JobPid -> #job{})
                cur_job_id = 1,
                job_pids = dict:new()  %% (JobId -> JobPid)
               }).

-record(job, {pid,
              id,
              from}).

-define(NAME, {global, ?MODULE}).
-define(DEFAULT_MODE, [{n_ram_copies, 2}]).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?NAME, ?MODULE, [], []).

new_job(Map, Reduce) ->
    new_job(Map, Reduce, ?DEFAULT_MODE).

new_job(Map, Reduce, Mode) ->
    new_job(Map, Reduce, Mode, infinity).

new_job(Map, Reduce, Mode, MaxTasks) ->
    gen_server:call(?NAME, {new_job, Map, Reduce, Mode, MaxTasks}, infinity).

new_job_from_another(OtherJobId, Map, Reduce) ->
    new_job_from_another(OtherJobId, Map, Reduce, ?DEFAULT_MODE).

new_job_from_another(OtherJobId, Map, Reduce, Mode) ->
    new_job_from_another(OtherJobId, Map, Reduce, Mode, infinity).

new_job_from_another(OtherJobId, Map, Reduce, Mode, MaxTasks) ->
    gen_server:call(?NAME, {new_job_from_another, OtherJobId, Map, Reduce,
                            Mode, MaxTasks}, infinity).

add_input(JobId, Input) ->
    gen_server:call(?NAME, {add_input, JobId, Input}, infinity).

import_from_output(JobId, PrevJobId) ->
    gen_server:call(?NAME, {import_from_output, JobId, PrevJobId}, infinity).

do_job(JobId) ->
    gen_server:call(?NAME, {do_job, JobId}, infinity).

next_result(JobId) ->
    gen_server:call(?NAME, {next_result, JobId}, infinity).

whole_result(JobId) ->
    case gen_server:call(?NAME, {next_result, JobId}, infinity) of
        end_of_result               -> [];
        Result when is_list(Result) -> Result ++ whole_result(JobId)
    end.

kill_job(JobId) ->
    gen_server:call(?NAME, {kill_job, JobId}, infinity).

%------------------------------------------------------------------------------
% Internal API
%------------------------------------------------------------------------------

job_finished(JobPid) ->
    gen_server:cast(?NAME, {job_finished, JobPid}).

%------------------------------------------------------------------------------
% Handlers
%------------------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call({new_job, Map, Reduce, Mode, MaxTasks}, _From, State) ->
    handle_new_job(direct, Map, Reduce, Mode, MaxTasks, State);
handle_call({new_job_from_another, OtherJobId, Map, Reduce, Mode, MaxTasks},
            _From, State = #state{job_pids = JobPids}) ->
    OtherJobPid = dict:fetch(OtherJobId, JobPids),
    OutputTable = smr_job:handover_output(OtherJobPid),
    handle_new_job({other_job, OutputTable}, Map, Reduce, Mode, MaxTasks,
                   State);
handle_call({add_input, JobId, Input}, _From,
            State = #state{job_pids = JobPids}) ->
    smr_job:add_input(dict:fetch(JobId, JobPids), Input),
    {reply, ok, State};
handle_call({import_from_output, JobId, PrevJobId}, _From,
            State = #state{job_pids = JobPids}) ->
    PrevJobPid = dict:fetch(PrevJobId, JobPids),
    JobPid = dict:fetch(JobId, JobPids),
    OutputTable = smr_job:handover_output(PrevJobPid),
    {reply, smr_job:import_from_output(JobPid, PrevJobId, OutputTable), State};
handle_call({do_job, JobId}, From, State = #state{job_pids = JobPids,
                                                  jobs = Jobs}) ->
    JobPid = dict:fetch(JobId, JobPids),
    Job = dict:fetch(JobPid, Jobs),
    smr_job:start(JobPid),
    {noreply,
     State#state{jobs = dict:store(JobPid, Job#job{from = From}, Jobs)}};
handle_call({next_result, JobId}, _From, State = #state{job_pids = JobPids}) ->
    JobPid = dict:fetch(JobId, JobPids),
    {reply, smr_job:next_result(JobPid), State};
handle_call({kill_job, JobId}, _From, State = #state{job_pids = JobPids}) ->
    case dict:find(JobId, JobPids) of
        {ok, JobPid} -> {reply, smr_job:kill(JobPid), State};
        error        -> {reply, {error, not_found}}
    end.

handle_cast({job_finished, JobPid}, State = #state{jobs = Jobs}) ->
    Job = #job{from = From, id = Id} = dict:fetch(JobPid, Jobs),
    error_logger:info_msg("MapReduce job ~p successfully completed~n", [Id]),
    gen_server:reply(From, ok),
    NewJobs = dict:store(JobPid, Job#job{from = none}, Jobs),
    {noreply, State#state{jobs = NewJobs}}.

handle_info({'DOWN', _, process, Pid, Reason}, State = #state{jobs = Jobs}) ->
    handle_job_exit(Pid, dict:fetch(Pid, Jobs), Reason, State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%------------------------------------------------------------------------------
% Internal
%------------------------------------------------------------------------------

handle_new_job(InputMode, Map, Reduce, Mode, MaxTasks,
               State = #state{jobs = Jobs,
                              cur_job_id = JobId,
                              job_pids = JobPids}) ->
    {ok, JobSup} = smr_job_sup_sup:start_job_sup(
                       InputMode,Map, Reduce, JobId, Mode, MaxTasks),
    JobPid = smr_job_sup:job(JobSup),
    erlang:monitor(process, JobPid),
    NewJobPids = dict:store(JobId, JobPid, JobPids),
    NewJobs = dict:store(JobPid, #job{pid = JobPid,
                                      id = JobId}, Jobs),
    {reply, {ok, JobId}, State#state{jobs = NewJobs,
                                     job_pids = NewJobPids,
                                     cur_job_id = JobId + 1}}.

handle_job_exit(Pid, #job{id = Id, from = From}, Reason,
                State = #state{jobs = Jobs, job_pids = JobPids}) ->
    case Reason of
        normal -> ok;
        _      -> error_logger:info_msg("MapReduce job ~p failed. Reason: ~p~n",
                                        [Id, Reason]),
                  smr_statistics:job_failed(Id, Reason),
                  case From of none -> ok;
                               _    -> gen_server:reply(From, {error, Reason})
                  end
    end,
    {noreply, State#state{jobs     = dict:erase(Pid, Jobs),
                          job_pids = dict:erase(Id, JobPids)}}.
