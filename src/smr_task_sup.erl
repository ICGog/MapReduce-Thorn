
-module(smr_task_sup).

-behaviour(supervisor_bridge).

-export([start_link/6]).
-export([init/1, terminate/2]).

%------------------------------------------------------------------------------

start_link(JobPid, TaskId, TaskType, TaskFun, FromTable, ToTable) ->
    supervisor_bridge:start_link(?MODULE, [JobPid, TaskId, TaskType, TaskFun,
                                           FromTable, ToTable]).

%------------------------------------------------------------------------------

init([JobPid, TaskId, TaskType, TaskFun, FromTable, ToTable]) ->
    Pid = smr_pool:pspawn_link(smr_task, TaskType,
                               [self(), JobPid, TaskId, TaskFun, FromTable,
                                ToTable]),
    {ok, Pid, Pid}.

terminate(_Reason, Pid) ->
    exit(Pid, kill).
