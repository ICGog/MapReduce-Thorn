
-module(smr_job_sup).

-behaviour(supervisor).

-export([start_link/0, start_job/5]).
-export([init/1]).

%------------------------------------------------------------------------------

start_link() ->
    supervisor:start_link(?MODULE, []).

start_job(Sup, Master, MapFun, ReduceFun, Input) ->
    supervisor:start_child(Sup, [Master, MapFun, ReduceFun, Input]).

%------------------------------------------------------------------------------

init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{job, {smr_job, start_link, []}, temporary, 60, worker,
            [smr_job]}]}}.
