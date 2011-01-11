
-module(smr_task_sup).

-behaviour(supervisor_bridge).

-export([start_link/7]).
-export([init/1, terminate/2]).

%------------------------------------------------------------------------------

start_link(TaskType, JobPid, LookupHash, TaskFun, FromTable, ToTable, Props) ->
    supervisor_bridge:start_link(?MODULE, [TaskType, JobPid, LookupHash,
                                           TaskFun, FromTable, ToTable, Props]).

%------------------------------------------------------------------------------

init([TaskType, JobPid, LookupHash, TaskFun, FromTable, ToTable, Props]) ->
    Pid = smr_task:start_link(TaskType, JobPid, LookupHash, TaskFun, FromTable,
                              ToTable, Props),
    {ok, Pid, Pid}.

terminate(_Reason, Pid) ->
    exit(Pid, kill).
