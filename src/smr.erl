
-module(smr).

-behaviour(application).

-export([start/0, stop/0]).
-export([start/2, stop/1]).

-include("smr.hrl").

%%---------------------------------------------------------------------------

start() ->
    application:start(smr).

stop() ->
    lists:foreach(fun smr_pool:detach_node/1, smr_pool:get_nodes()),
    ok = smr_mnesia:stop(),
    ok = application:stop(smr).

%%---------------------------------------------------------------------------

start(_StartType, [EnableWebsite]) ->
    {ok, Sup} = smr_sup:start_link(),
    case EnableWebsite of true  -> smr_http:start();
                          false -> ok
    end,
    error_logger:info_msg("~nSmashing MapReduce is starting...~n", []),
    smr_mnesia:start(),
    AutoAttachOutcome = smr_pool:auto_attach_nodes(),
    error_logger:info_msg("Auto attach nodes outcome: ~w~n",
                          [AutoAttachOutcome]),
    error_logger:info_msg("~nSmashing MapReduce running!~n~n~n", []),
    {ok, Sup}.

stop(_State) ->
    ok.
