
-module(smr_task_sup_sup).

-behaviour(supervisor).

-export([start_link/0, start_task/8]).
-export([init/1]).

%------------------------------------------------------------------------------

start_link() ->
    supervisor:start_link(?MODULE, []).

start_task(Sup, TaskType, JobPid, LookupHash, TaskFun, FromTable, ToTable,
           Props) ->
    supervisor:start_child(Sup, [TaskType, JobPid, LookupHash, TaskFun,
                                 FromTable, ToTable, Props]).

%------------------------------------------------------------------------------

init([]) ->
    {ok, {{simple_one_for_one, 10, 60 * 60},
          [{task_sup, {smr_task_sup, start_link, []}, transient,
            60, supervisor, [smr_task_sup]}]}}.
