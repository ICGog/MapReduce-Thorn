
-module(smr_sup).

-behaviour(supervisor).

-export([start_link/1, master/1, statistics/1, job_sup_sup/1]).
-export([init/1]).

%------------------------------------------------------------------------------

start_link(Nodes) ->
    supervisor:start_link(?MODULE, [Nodes]).

master(Sup) ->
    child_pid(Sup, master).

statistics(Sup) ->
    child_pid(Sup, statistics).

job_sup_sup(Sup) ->
    child_pid(Sup, job_sup_sup).

%------------------------------------------------------------------------------

init([Nodes]) ->
    {ok, {{rest_for_one, 2, 60},
          [{master, {smr_master, start_link, []}, transient, 60,
            worker, [smr_master]},
           {job_sup_sup, {smr_job_sup_sup, start_link, []}, transient, infinity,
            supervisor, [smr_job_sup]},
           {statistics, {smr_statistics, start_link, [Nodes]}, transient, 60,
            worker, [smr_statistics]}]}}.

%------------------------------------------------------------------------------

child_pid(Sup, Name) ->
    {_, Pid, _, _} = lists:keyfind(Name, 1, supervisor:which_children(Sup)),
    Pid.
