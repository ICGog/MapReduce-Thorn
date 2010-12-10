
-module(smr_task).

-export([do/7, run_thorn_job/2]).

-include("smr.hrl").

%------------------------------------------------------------------------------

do(Type, Job, LookupHash, Fun, FromTable, ToTable, Props) ->
    SubProcs = proplists:get_value(worker_sub_processes, Props,
                                   ?WORKER_SUB_PROCESSES),
    What = case Type of map           -> input;
                        map_no_reduce -> input_no_reduce;
                        reduce        -> inter
           end,
    {Hashes, ResultSize} =
        smr_mnesia:process(
            What,
            [LookupHash,
             fun (Input) ->
                     smr_job:task_started(Job, self(), LookupHash,
                                          erts_debug:flat_size(Input)),
                     case Type of
                         reduce ->
                             plists:map(fun (KVs = {K, _}) ->
                                                {K, Fun(KVs)}
                                        end, Input, {processes, SubProcs});
                         _Map ->
                             lists:flatten(plists:map(Fun, Input,
                                                      {processes, SubProcs}))
                     end
             end,
             FromTable, ToTable, Props]),
    smr_job:task_finished(Job, self(), LookupHash, Hashes, ResultSize).

run_thorn_job(Code, Input) ->
    Temp = os:cmd("mktemp /tmp/XXXXXXXXXX-tmp.th"),
    EscCode = re:replace(Code, "'", "'\"'\"'", [{return, list}, global]),
    os:cmd("echo '" ++ EscCode ++ "' > " ++ Temp),
    EscInput = re:replace(Input, "'", "'\"'\"'", [{return, list}, global]),
    Output = os:cmd("echo '" ++ EscInput ++ "' | $TH -f " ++ Temp),
    os:cmd("rm -f " ++ Temp),
    Output.
