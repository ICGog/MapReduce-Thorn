
-module(smr_job_sup_sup).

-behaviour(supervisor).

-export([start_link/0, start_job_sup/3]).
-export([init/1]).

-define(NAME, {global, ?MODULE}).

%------------------------------------------------------------------------------

start_link() ->
    supervisor:start_link(?NAME, ?MODULE, []).

start_job_sup(MapFun, ReduceFun, JobId) ->
    supervisor:start_child(?NAME, [MapFun, ReduceFun, JobId]).

%------------------------------------------------------------------------------

init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{job_sup, {smr_job_sup, start_link, []}, temporary, 60, worker,
            [smr_job_sup]}]}}.
