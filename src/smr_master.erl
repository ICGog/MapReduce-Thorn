
-module(smr_master).

-behavior(gen_server).

-export([start_link/0, map_reduce/4]).
-export([job_result/3]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, 
        code_change/3]).

-record(state, {sup,
                jobs = dict:new()}).

-record(job, {pid, from, map_fun, reduce_fun}).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({global, smr_master}, ?MODULE, [self()], []).

map_reduce(Pid, Map, Reduce, Input) -> 
    gen_server:call(Pid, {map_reduce, Map, Reduce, Input}, infinity).

%------------------------------------------------------------------------------
% Internal API
%------------------------------------------------------------------------------

job_result(Pid, JobPid, Result) ->
    gen_server:cast(Pid, {job_result, JobPid, Result}).

%------------------------------------------------------------------------------
% Handlers
%------------------------------------------------------------------------------

init([Sup]) ->
    {ok, #state{sup = Sup}}.

handle_call({map_reduce, Map, Reduce, Input}, From, 
            State = #state{sup = Sup, jobs = Jobs}) ->
    {ok, JobSup} = smr_job_sup_sup:start_job_sup(smr_sup:job_sup_sup(Sup),
                                                 self(), Map, Reduce, Input),
    JobPid = smr_job_sup:job(JobSup),
    erlang:monitor(process, JobPid),
    {noreply, State#state{jobs = dict:store(JobPid, #job{pid = JobPid,
                                                         from = From,
                                                         map_fun = Map,
                                                         reduce_fun = Reduce},
                                            Jobs)}}.

handle_cast({job_result, JobPid, Result}, State = #state{jobs = Jobs}) ->
    error_logger:info_msg("MapReduce job ~p successfully completed~n",
                          [JobPid]),
    gen_server:reply((dict:fetch(JobPid, Jobs))#job.from, {ok, Result}),
    {noreply, State}.

handle_info({'DOWN', _, process, Pid, Reason}, State = #state{jobs = Jobs}) ->
    true = dict:is_key(Pid, Jobs), %% assertion
    handle_job_exit(Pid, Reason, State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%------------------------------------------------------------------------------
% Internal
%------------------------------------------------------------------------------

handle_job_exit(Pid, Reason, State = #state{jobs = Jobs}) ->
    case Reason of
        normal ->
            ok;
        _ ->
            error_logger:info_msg("MapReduce job ~p failed. Reason: ~p~n",
                                  [Pid, Reason]),
            gen_server:reply((dict:fetch(Pid, Jobs))#job.from, {error, Reason})
    end,
    {noreply, State#state{jobs = dict:erase(Pid, Jobs)}}.
