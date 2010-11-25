
-module(smr_sup).

-behaviour(supervisor).

-export([start_link/0, master/1, statistics/1, worker_sup/1, job_sup/1]).
-export([init/1]).

%------------------------------------------------------------------------------

start_link() ->
    supervisor:start_link(?MODULE, []).

master(Sup) ->
    child_pid(Sup, master).

statistics(Sup) ->
    child_pid(Sup, statistics).

worker_sup(Sup) ->
    child_pid(Sup, worker_sup).

job_sup(Sup) ->
    child_pid(Sup, job_sup).

%------------------------------------------------------------------------------

init([]) ->
    {ok, {{rest_for_one, 2, 60},
          [{master, {smr_master, start_link, []}, transient, 60,
            worker, [smr_master]},
           {job_sup, {smr_job_sup, start_link, []}, transient, infinity,
            supervisor, [smr_job_sup]},
           {worker_sup, {smr_worker_sup, start_link, []}, transient, infinity,
            supervisor, [smr_worker_sup]},
           {statistics, {smr_statistics, start_link, []}, transient, 60,
            worker, [smr_statistics]}]}}.

%------------------------------------------------------------------------------

child_pid(Sup, Name) ->
    {_, Pid, _, _} = lists:keyfind(Name, 1, supervisor:which_children(Sup)),
    Pid.
