-module(smr_worker).
-behaviour(gen_server).

-export([do_job/5]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2,
        code_change/3]).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

do_job(Worker, Job, JobType, Fun, Input) ->
    gen_server:cast(Worker, {do_job, Job, JobType, Fun, Input}).

%------------------------------------------------------------------------------
% Handlers
%------------------------------------------------------------------------------

init([]) ->
    {ok, none}.

handle_call(_Request, _From, State) -> {stop, unexpected_call, State}.

handle_cast({do_job, Job, map, MapFun, MapInput}, State) ->
    smr_job:result(Job, self(), lists:flatmap(MapFun, MapInput)),
    {noreply, State};
handle_cast({do_job, Job, reduce, ReduceFun, ReduceInput}, State) ->
    smr_job:result(Job, self(),
                   lists:map(fun (KV = {K, _}) -> {K, ReduceFun(KV)} end,
                             ReduceInput)),
    {noreply, State}.

handle_info(_Message, State) -> {stop, unexpected_message, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.
