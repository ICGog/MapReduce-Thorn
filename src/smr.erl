
-module(smr).

-behaviour(application).

-export([start/0, stop/0, master/0, statistics/0]).
-export([start/2, stop/1]).

%%---------------------------------------------------------------------------

start() ->
    application:start(smr).

stop() ->
    application:stop(smr).

master() ->
    global:whereis_name(smr_master).

statistics() ->
    global:whereis_name(smr_statistics).

%%---------------------------------------------------------------------------

start(_StartType, [EnableWebsite]) ->
    {ok, Sup} = smr_sup:start_link(),
    case EnableWebsite of true  -> smr_http:start();
                          false -> ok
    end,
    {ok, Sup}.

stop(_State) ->
    ok.
