-module(map_reduce).
-export([map_reduce/4]).

map_reduce(Map, Reduce, Acc, Input) ->
    Sid = self(),
    Pid = spawn(fun() -> reduce(Sid, Map, Reduce, Acc, Input) end),
    receive {Pid, Result} -> Result
    end.

reduce(Parent, Map, Reduce, Acc, Input) ->
    process_flag(trap_exit, true),
    ReducePid = self(),
    lists:foreach(fun(SingleInput) ->
                spawn_link(fun() -> Map(ReducePid, SingleInput) end)
            end, Input),
    N = length(Input),
    ReplyDict = get_values(N, dict:new()),
    ResAcc = dict:fold(Reduce, Acc, ReplyDict),
    Parent ! {self(), ResAcc}.

get_values(0, Dict) ->
    Dict;
get_values(N, Dict) ->
    receive 
        {Key, Val} ->
            case dict:is_key(Key, Dict) of
                true ->
                    NewDict = dict:append(Key, Val, Dict),
                    get_values(N, NewDict);
                false ->
                    NewDict = dict:store(Key, [Val], Dict),
                    get_values(N, NewDict)
            end;
        {'EXIT', _, Why} ->
            get_values(N - 1, Dict)
    end.
