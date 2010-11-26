
-module(smr_task).

-export([map/4, reduce/4, run_thorn_job/2]).

%------------------------------------------------------------------------------

map(Sup, Job, MapFun, Input) ->
    receive start -> ok end,
    smr_job:result(Job, Sup, lists:flatmap(MapFun, Input)),
    ok.

reduce(Sup, Job, ReduceFun, Input) ->
    receive start -> ok end,
    smr_job:result(Job, Sup,
                   lists:map(fun (KV = {K, _}) -> {K, ReduceFun(KV)} end,
                             Input)),
    ok.

run_thorn_job(Code, Input) ->
    Temp = os:cmd("mktemp /tmp/XXXXXXXXXX-tmp.th"),
    EscCode = re:replace(Code, "'", "'\"'\"'", [{return, list}, global]),
    os:cmd("echo '" ++ EscCode ++ "' > " ++ Temp),
    EscInput = re:replace(Input, "'", "'\"'\"'", [{return, list}, global]),
    Output = os:cmd("echo '" ++ EscInput ++ "' | $TH -f " ++ Temp),
    os:cmd("rm -f " ++ Temp),
    Output.
