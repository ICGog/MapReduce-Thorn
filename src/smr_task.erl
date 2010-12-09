
-module(smr_task).

-export([map/6, reduce/6, run_thorn_job/2]).

-include("smr.hrl").

%------------------------------------------------------------------------------

map(_Sup, Job, LookupHash, MapFun, FromTable, ToTable) ->
    {Hashes, ResultSize} =
        smr_mnesia:process_input(
            LookupHash,
            fun (Input) ->
                    smr_job:task_started(Job, self(), LookupHash,
                                         erts_debug:flat_size(Input)),
                    lists:flatten(
                        plists:map(MapFun, Input,
                                   {processes, ?WORKER_SUB_PROCESSES}))
            end,
            FromTable, ToTable),
    smr_job:task_finished(Job, self(), LookupHash, Hashes, ResultSize).

reduce(_Sup, Job, LookupHash, ReduceFun, FromTable, ToTable) ->
    {Hashes, ResultSize} =
        smr_mnesia:process_inter(
            LookupHash,
            fun (KVsList) ->
                    smr_job:task_started(Job, self(), LookupHash,
                                         erts_debug:flat_size(KVsList)),
                    plists:map(fun (KVs = {K, _}) -> {K, ReduceFun(KVs)} end,
                               KVsList, {processes, ?WORKER_SUB_PROCESSES})
            end,
            FromTable, ToTable),
    smr_job:task_finished(Job, self(), LookupHash, Hashes, ResultSize).

run_thorn_job(Code, Input) ->
    Temp = os:cmd("mktemp /tmp/XXXXXXXXXX-tmp.th"),
    EscCode = re:replace(Code, "'", "'\"'\"'", [{return, list}, global]),
    os:cmd("echo '" ++ EscCode ++ "' > " ++ Temp),
    EscInput = re:replace(Input, "'", "'\"'\"'", [{return, list}, global]),
    Output = os:cmd("echo '" ++ EscInput ++ "' | $TH -f " ++ Temp),
    os:cmd("rm -f " ++ Temp),
    Output.
