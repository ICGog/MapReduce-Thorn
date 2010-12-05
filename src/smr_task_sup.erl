
-module(smr_task_sup).

-behaviour(supervisor_bridge).

-export([start_link/4]).
-export([init/1, terminate/2]).

%------------------------------------------------------------------------------

start_link(JobPid, TaskType, TaskFun, Input) ->
    supervisor_bridge:start_link(?MODULE, [JobPid, TaskType, TaskFun, Input]).

%------------------------------------------------------------------------------

init([JobPid, TaskType, TaskFun, Input]) ->
    Pid = smr_pool:pspawn_link(smr_task, TaskType,
                               [self(), JobPid, TaskFun, Input]),
    {ok, Pid, Pid}.

terminate(shutdown, Pid) ->
    exit(Pid, kill).
