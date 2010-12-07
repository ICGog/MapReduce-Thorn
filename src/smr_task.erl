
-module(smr_task).

-export([map/6, reduce/6, run_thorn_job/2]).

%------------------------------------------------------------------------------

map(_Sup, Job, TaskId, MapFun, FromTable, ToTable) ->
    Processes = 4,
    ExpectedReduceKeys = infinity,
    HashFun = case ExpectedReduceKeys of
                  infinity -> fun erlang:phash2/1;
                  _        -> MaxHash = ExpectedReduceKeys div Processes,
                              fun (Key) -> erlang:phash2(Key, MaxHash) end
              end,
    {Hashes, ResultSize} =
        smr_mnesia:process_input(
            TaskId,
            fun (Input) ->
                    smr_job:task_started(Job, self(), TaskId,
                                         erts_debug:flat_size(Input)),
                    lists:flatten(plists:map(MapFun, Input,
                                             {processes, Processes}))
            end,
            HashFun, FromTable, ToTable),
    smr_job:task_finished(Job, self(), TaskId, [Hashes, ResultSize]).

reduce(_Sup, Job, TaskId, ReduceFun, FromTable, ToTable) ->
    Processes = 4,
    smr_mnesia:process_inter(
        TaskId,
        fun (KVsList) ->
                smr_job:task_started(Job, self(), TaskId,
                                     erts_debug:flat_size(KVsList)),
                plists:map(fun (KVs = {K, _}) -> {K, ReduceFun(KVs)} end,
                           KVsList, {processes, Processes})
        end,
        FromTable, ToTable),
    smr_job:task_finished(Job, self(), TaskId, []).

run_thorn_job(Code, Input) ->
    Temp = os:cmd("mktemp /tmp/XXXXXXXXXX-tmp.th"),
    EscCode = re:replace(Code, "'", "'\"'\"'", [{return, list}, global]),
    os:cmd("echo '" ++ EscCode ++ "' > " ++ Temp),
    EscInput = re:replace(Input, "'", "'\"'\"'", [{return, list}, global]),
    Output = os:cmd("echo '" ++ EscInput ++ "' | $TH -f " ++ Temp),
    os:cmd("rm -f " ++ Temp),
    Output.
