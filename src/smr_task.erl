
-module(smr_task).

-export([map/4, reduce/4, run_thorn_job/2]).

%------------------------------------------------------------------------------

map(_Sup, Job, MapFun, Input) ->
    smr_job:task_started(Job, self(), length(Input)),
    smr_job:result(Job, self(), lists:flatmap(MapFun, Input)).

reduce(_Sup, Job, ReduceFun, Input) ->
    smr_job:task_started(Job, self(), length(Input)),
    smr_job:result(Job, self(),
                   lists:map(fun (KV = {K, _}) -> {K, ReduceFun(KV)} end,
                             Input)).

run_thorn_job(Code, Input) ->
    Temp = os:cmd("mktemp /tmp/XXXXXXXXXX-tmp.th"),
    EscCode = re:replace(Code, "'", "'\"'\"'", [{return, list}, global]),
    os:cmd("echo '" ++ EscCode ++ "' > " ++ Temp),
    EscInput = re:replace(Input, "'", "'\"'\"'", [{return, list}, global]),
    Output = os:cmd("echo '" ++ EscInput ++ "' | $TH -f " ++ Temp),
    os:cmd("rm -f " ++ Temp),
    Output.
