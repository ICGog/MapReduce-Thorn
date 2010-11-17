-module(integration).

-export([double/1, add_values/0, start/0]).

add_values() -> 
    receive
        add -> io:format("Must add!!!~n",[]),
               add_values()
    end.

start() ->
 register(add_test, spawn(test, add_values, [])).

double(X) -> 2 * X.
