-module(integration).

-export([getBytesFromThorn/3, sendBytesToThorn/1, getStringsFromThorn/3, sendStringsToThorn/1]).
-export([double/1, add_values/0, start/0]).

add_values() -> 
    receive
        add -> io:format("Must add!!!~n",[]),
               add_values()
    end.

start() ->
 register(add_test, spawn(test, add_values, [])).

double(X) -> 2 * X.

getBytesFromThorn(Map,Reduce,KvpList) -> io:format("~p~n~p~n~p~n", [Map,Reduce,KvpList]).

sendBytesToThorn(_X) -> [].

getStringsFromThorn(Map,Reduce,KvpList) -> io:format("~p~n~p~n~p~n", [Map,Reduce,KvpList]).

sendStringsToThorn(_X) -> [{"A","10"}, {"B","90"}, {"C","100"}].


