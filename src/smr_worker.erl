
-module(smr_worker).

-export([get_input/0, send_output/1, run_thorn_job/2]).

get_input() ->
    smr_tasklet ! {get_input, self()},
    receive Result -> Result end.

send_output(Output) ->
    smr_tasklet ! {output, self(), Output},
    receive R -> R = ok end.

run_thorn_job(FunCode, Type) ->
    ThornCode = thorn_worker_code(FunCode, Type),
    spawn_thorn_interpretor(ThornCode, "").

spawn_thorn_interpretor(Code, Input) ->
    Temp = os:cmd("mktemp /tmp/th-tmp-XXXXXXXXXX"),
    EscCode = re:replace(Code, "'", "'\"'\"'", [{return, list}, global]),
    os:cmd("echo '" ++ EscCode ++ "' > " ++ Temp),
    EscInput = re:replace(Input, "'", "'\"'\"'", [{return, list}, global]),
    Output = os:cmd("echo '" ++ EscInput ++ "' | $TH -f " ++ Temp),
    os:cmd("rm -f " ++ Temp),
    case Output of "ok\n" -> ok;
                   _      -> exit({unexpected_thorn_output, Output})
    end.

thorn_worker_code(FunCode, Type) ->
    "\n import MAPREDUCE.*;" ++
    "\n" ++
    "\n fun doTask(type, peer, cookie) {" ++
    "\n     var input := getInput(type, peer, cookie);" ++
    "\n     var output := [];" ++
    case Type of
        reduce ->
            "\n         for ([k, v] <- input) {" ++
            "\n             output := [[k, reduceFun([k, v])], output...];" ++
            "\n         }";
        _Map ->
            "\n         for (tuple <- input) {" ++
            "\n             for(resTuple <- mapFun(tuple)) {" ++
            "\n                 output := [resTuple, output...];" ++
            "\n             }" ++
            "\n         }"
    end ++
    "\n     sendOutput(output);" ++
    "\n }" ++
    "\n " ++
    "\n " ++ FunCode ++
    "\n " ++
    "\n var cookie := \"" ++ atom_to_list(erlang:get_cookie()) ++ "\";" ++
    "\n var peer := \"" ++ atom_to_list(node()) ++ "\";" ++
    "\n var type := \"" ++ case Type of reduce -> "reduce";
                                        _Map   -> "map"
                           end ++ "\";"
    "\n " ++
    "\n doTask(type, peer, cookie);" ++
    "\n println(\"ok\");" ++
    "\n ".

