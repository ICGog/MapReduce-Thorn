
-module(smr_map_reduce).

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
    Parent ! {self(), 
        dict:fold(Reduce, Acc, get_values(length(Input), dict:new()))}.

get_values(0, Dict) ->
    Dict;
get_values(N, Dict) ->
    receive 
        {Key, Val} ->
            case dict:is_key(Key, Dict) of
                true ->
                    get_values(N, dict:apped(Key, Val, Dict));
                false ->
                    get_values(N, dict:store(Key, [Val], Dict))
            end;
        {'EXIT', _, normal} ->
            get_values(N - 1, Dict)
    end.
