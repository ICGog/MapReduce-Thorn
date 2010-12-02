
-module(smr_task_sup).

-behaviour(supervisor_bridge).

-export([start_link/4]).
-export([init/1, terminate/2]).

%------------------------------------------------------------------------------

start_link(JobPid, TaskType, TaskFun, Input) ->
    supervisor_bridge:start_link(?MODULE, [JobPid, TaskType, TaskFun, Input]).

%------------------------------------------------------------------------------

init([JobPid, TaskType, TaskFun, Input]) ->
    Pid = spawn_no_master(JobPid, TaskType, TaskFun, Input),
    link(Pid),
    smr_job:batch_started(JobPid, Pid),
    Pid ! start,
    {ok, Pid, Pid}.

spawn_no_master(JobPid, TaskType, TaskFun, Input) ->
    Pid = pool:pspawn(smr_task, TaskType, [self(), JobPid, TaskFun, Input]),
    Master = node(smr:master()),
    case node(Pid) of 
        Master -> exit(Pid, kill),
                  spawn_no_master(JobPid, TaskType, TaskFun, Input);
        _      -> Pid
    end.

terminate(shutdown, Pid) ->
    exit(Pid, kill).
