
-module(smr_worker).

-behaviour(gen_server).

-export([start_link/1, do_job/5]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2,
        code_change/3]).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

start_link(Node) ->
    {ok, proc_lib:spawn_link(
             Node,
             fun () ->
                     {ok, State} = ?MODULE:init([]),
                     gen_server:enter_loop(?MODULE, [], State)
             end)}.

do_job(Worker, JobPid, JobType, Fun, Input) ->
    gen_server:cast(Worker, {do_job, JobPid, JobType, Fun, Input}).

%------------------------------------------------------------------------------
% Handlers
%------------------------------------------------------------------------------

init([]) ->
    {ok, none}.

handle_call(Call, _From, State) ->
    {stop, {unexpected_call, Call}, State}.

handle_cast({do_job, JobPid, map, MapFun, MapInput}, State) ->
    smr_job:result(JobPid, self(), lists:flatmap(MapFun, MapInput)),
    {noreply, State};
handle_cast({do_job, JobPid, reduce, ReduceFun, ReduceInput}, State) ->
    smr_job:result(JobPid, self(),
                   lists:map(fun (KV = {K, _}) -> {K, ReduceFun(KV)} end,
                             ReduceInput)),
    {noreply, State}.

handle_info(Msg, State) ->
    {stop, {unexpected_message, Msg}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
