-module(smr_worker).
-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2,
        code_change/3]).

%------------------------------------------------------------------------------
% Handlers
%------------------------------------------------------------------------------

init(_Args) ->
    {ok, []}.

handle_call(_Request, _From, State) -> {stop, unexpected_call, State}.

handle_cast(_Request, State) -> {stop, unexpected_cast, State}.

handle_info(Message, State) -> {stop, unexpected_message, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

