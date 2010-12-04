
-module(smr_job_sup).

-behaviour(supervisor).

-export([start_link/3, job/1, task_sup_sup/1]).
-export([init/1]).

%------------------------------------------------------------------------------

start_link(MapFun, ReduceFun, JobId) ->
    supervisor:start_link(?MODULE, [MapFun, ReduceFun, JobId]).

job(Sup) ->
    child_pid(Sup, job).

task_sup_sup(Sup) ->
    child_pid(Sup, task_sup_sup).

%------------------------------------------------------------------------------

init([MapFun, ReduceFun, JobId]) ->
    {ok, {{one_for_all, 0, 1},
          [{job, {smr_job, start_link, [MapFun, ReduceFun, JobId]},
            permanent, 60, worker, [smr_job]},
           {task_sup_sup, {smr_task_sup_sup, start_link, []},
            permanent, infinity, supervisor, [smr_task_sup_sup]}]}}.

%------------------------------------------------------------------------------

child_pid(Sup, Name) ->
    {_, Pid, _, _} = lists:keyfind(Name, 1, supervisor:which_children(Sup)),
    Pid.
