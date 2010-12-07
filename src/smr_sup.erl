
-module(smr_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(NAME, {global, ?MODULE}).

%------------------------------------------------------------------------------

start_link() ->
    supervisor:start_link(?NAME, ?MODULE, []).

%------------------------------------------------------------------------------

init([]) ->
    {ok, {{one_for_all, 0, 1},
          [{pool, {smr_pool, start_link, []}, permanent, 60,
            worker, [smr_pool]},
           {master, {smr_master, start_link, []}, permanent, 60,
            worker, [smr_master]},
           {job_sup_sup, {smr_job_sup_sup, start_link, []}, permanent, infinity,
            supervisor, [smr_job_sup]},
           {statistics, {smr_statistics, start_link, []}, permanent, 60,
            worker, [smr_statistics]}]}}.
