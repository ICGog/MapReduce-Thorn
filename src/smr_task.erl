
-module(smr_task).

-export([start_link/7, do/8]).

-include("smr.hrl").

%------------------------------------------------------------------------------

start_link(Type, Job, LookupHash, Fun, FromTable, ToTable, Props) ->
    spawn_link(fun () -> process_flag(trap_exit, true),
                         spawn_loop([Type, Job, LookupHash, Fun, FromTable,
                                     ToTable, Props], 0, none, false)
               end).

spawn_loop(_, 10, LastReason, _Purge) ->
    exit({failed_too_many_times, {last_reason, LastReason}});
spawn_loop(Args, Failures, _LastReason, Purge) ->
    Pid = smr_pool:pspawn_link(smr_task, do, Args ++ [Purge]),
    receive
        {'EXIT', Pid, normal} ->
            ok;
        {'EXIT', Pid, Reason} ->
            spawn_loop(Args, Failures + 1, Reason, true);
        {'EXIT', _, Reason} ->
            exit(Reason)
    end.

do(Type, Job, LookupHash, Fun, FromTable, ToTable, Props, Purge) ->
    register(smr_tasklet, self()),
    case Purge of
        true  -> smr_mnesia:purge_chunks_with_prev_hash(LookupHash, ToTable);
        false -> ok
    end,
    What = case Type of map           -> input;
                        map_no_reduce -> input_no_reduce;
                        reduce        -> inter
           end,
    Lang = proplists:get_value(worker_lang, Props, erl_fun),
    {Hashes, ResultSize} =
        smr_mnesia:process(
            What,
            [LookupHash,
             fun (Input) ->
                     smr_job:task_started(Job, self(), LookupHash,
                                          erts_debug:flat_size(Input)),
                     work(Lang, Type, Input, Fun, Props)
             end,
             FromTable, ToTable, Props]),
    smr_job:task_finished(Job, self(), LookupHash, Hashes, ResultSize).

work(erl_fun, Type, Input, Fun, Props) ->
    SubProcs = proplists:get_value(worker_sub_processes, Props,
                                   ?WORKER_SUB_PROCESSES),
    case Type of
        reduce -> plists:map(fun (KVs = {K, _}) -> {K, Fun(KVs)} end, Input,
                             {processes, SubProcs});
        _Map   -> lists:flatten(plists:map(Fun, Input, {processes, SubProcs}))
    end;
work(thorn, Type, Input, Fun, _Props) ->
    spawn_link(fun () -> smr_worker:run_thorn_job(Fun, Type) end),
    receive {get_input, Caller1} -> Caller1 ! {ok, Input} end,
    receive {output, Caller2, Output} -> Caller2 ! ok,
                                         Output
    end.

