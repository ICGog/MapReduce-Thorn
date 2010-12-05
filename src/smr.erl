
-module(smr).

-behaviour(application).

-export([start/0, stop/0]).
-export([start/2, stop/1]).

%%---------------------------------------------------------------------------

start() ->
    application:start(smr).

stop() ->
    application:stop(smr).

%%---------------------------------------------------------------------------

start(_StartType, [EnableWebsite]) ->
    {ok, Sup} = smr_sup:start_link(),
    case EnableWebsite of true  -> smr_http:start();
                          false -> ok
    end,
    AutoAttachOutcome = smr_pool:auto_attach_nodes(),
    error_logger:info_msg("Auto attach nodes outcome: ~w~n",
                          [AutoAttachOutcome]),
    {ok, Sup}.

stop(_State) ->
    ok.
