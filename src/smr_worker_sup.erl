
-module(smr_worker_sup).

-behaviour(supervisor).

-export([start_link/0, start_worker/2]).
-export([init/1]).

%------------------------------------------------------------------------------

start_link() ->
    supervisor:start_link(?MODULE, []).

start_worker(Sup, Node) ->
    supervisor:start_child(Sup, [Node]).

%------------------------------------------------------------------------------

init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{worker, {smr_worker, start_link, []}, temporary, brutal_kill,
            worker, [smr_worker]}]}}.
