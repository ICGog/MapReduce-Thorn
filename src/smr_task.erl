
-module(smr_task).

-export([map/4, reduce/4, run_thorn_job/3]).

%------------------------------------------------------------------------------

map(Sup, Job, MapFun, Input) ->
    receive start -> ok end,
    smr_statistics:worker_batch_started(smr:statistics(), node(self()), map),
    smr_job:result(Job, Sup, lists:flatmap(MapFun, Input)),
    smr_statistics:worker_batch_ended(smr:statistics(), node(self())),
    ok.

reduce(Sup, Job, ReduceFun, Input) ->
    receive start -> ok end,
    smr_statistics:worker_batch_started(smr:statistics(), node(self()), reduce),
    smr_job:result(Job, Sup,
                   lists:map(fun (KV = {K, _}) -> {K, ReduceFun(KV)} end,
                             Input)),
    smr_statistics:worker_batch_ended(smr:statistics(), node(self())),
    ok.

run_thorn_job(TaskType, Code, Input) ->
    smr_statistics:worker_batch_started(smr:statistics(), node(self()),
                                        TaskType),
    Temp = os:cmd("mktemp /tmp/XXXXXXXXXX-tmp.th"),
    EscCode = re:replace(Code, "'", "'\"'\"'", [{return, list}, global]),
    os:cmd("echo '" ++ EscCode ++ "' > " ++ Temp),
    EscInput = re:replace(Input, "'", "'\"'\"'", [{return, list}, global]),
    Output = os:cmd("echo '" ++ EscInput ++ "' | $TH -f " ++ Temp),
    os:cmd("rm -f " ++ Temp),
    smr_statistics:worker_batch_ended(smr:statistics(), node(self())),
    Output.
