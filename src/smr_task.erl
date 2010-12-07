
-module(smr_task).

-export([map/6, reduce/6, run_thorn_job/2]).

%------------------------------------------------------------------------------

map(_Sup, Job, TaskId, MapFun, FromTable, ToTable) ->
    {Hashes, ResultSize} =
        smr_mnesia:process_input(
            TaskId,
            fun (Input) ->
                    %% TODO calling task_started too late !!!! :|
                    smr_job:task_started(Job, self(), TaskId,
                                         erts_debug:flat_size(Input)),
                    lists:flatmap(MapFun, Input) end,
            FromTable, ToTable),
    smr_job:task_finished(Job, self(), TaskId, [Hashes, ResultSize]).

reduce(_Sup, Job, TaskId, ReduceFun, FromTable, ToTable) ->
    smr_job:task_started(Job, self(), TaskId, 5000), %% TODO size
    smr_mnesia:process_inter(TaskId, ReduceFun, FromTable, ToTable),
    smr_job:task_finished(Job, self(), TaskId, []).

run_thorn_job(Code, Input) ->
    Temp = os:cmd("mktemp /tmp/XXXXXXXXXX-tmp.th"),
    EscCode = re:replace(Code, "'", "'\"'\"'", [{return, list}, global]),
    os:cmd("echo '" ++ EscCode ++ "' > " ++ Temp),
    EscInput = re:replace(Input, "'", "'\"'\"'", [{return, list}, global]),
    Output = os:cmd("echo '" ++ EscInput ++ "' | $TH -f " ++ Temp),
    os:cmd("rm -f " ++ Temp),
    Output.
