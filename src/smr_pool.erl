
-module(smr_pool).

-behaviour(gen_server).

-export([start_link/0, attach_node/1, kill_node/1, kill_all_nodes/0, pspawn/3,
         pspawn_link/3]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2,
         code_change/3]).
-export([worker_init/3]).

-record(state, {nodes = dict:new(),       %% Node -> #node{}
                requests = queue:new(),
                free_nodes = sets:new()}).

-record(node, {node,
               task_pid = none}).

-define(NAME, {global, ?MODULE}).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

attach_node(Node) ->
    gen_server:call(?NAME, {attach, Node}, infinity).

kill_node(Node) ->
    gen_server:call(?NAME, {kill, Node}, infinity).

kill_all_nodes() ->
    gen_server:call(?NAME, kill_all, infinity).

%------------------------------------------------------------------------------
% Internal API
%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?NAME, ?MODULE, [], []).

pspawn(Module, Function, Args) ->
    pspawn(Module, Function, Args, false).

pspawn_link(Module, Function, Args) ->
    pspawn(Module, Function, Args, {true, self()}).

pspawn(Module, Function, Args, Link) ->
    gen_server:call(?NAME,
                    {spawn, {Module, Function, Args}, Link}, infinity).

%------------------------------------------------------------------------------
% Handlers
%------------------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call({attach, N}, From, State0 = #state{nodes = Ns,
                                               free_nodes = FNs}) ->
    case dict:is_key(N, Ns) of
        true  -> {reply, {error, already_attached}, State0};
        false -> case net_adm:ping(N) of
                     pong -> smr_statistics:register_worker(N),
                             gen_server:reply(From, attached),
                             monitor_node(N, true),
                             NewNs = dict:store(N, #node{node = N}, Ns),
                             NewFNs = sets:add_element(N, FNs),
                             State1 = maybe_serve(
                                          State0#state{nodes = NewNs,
                                                       free_nodes = NewFNs}),
                             {noreply, State1};
                     _    -> {reply, {error, no_connection}, State0}
                 end
    end;
handle_call({kill, N}, _From, State = #state{nodes = Ns}) ->
    case dict:is_key(N, Ns) of
        true  -> error_logger:info_msg("Killing node ~p~n", [N]),
                 {Reply, NewState} = kill_node(N, State),
                 {reply, Reply, NewState};
        false -> {reply, {error, node_not_in_pool}, State}
    end;
handle_call(kill_all, _From, State = #state{nodes = Ns}) ->
    error_logger:info_msg("Brutally killing all nodes~n", []),
    FinalState = dict:fold(fun (N, _, CurState) ->
                                   quick_kill_node(N, CurState)
                           end, State, Ns),
    {reply, ok, FinalState};
handle_call({spawn, _MFA, _Link} = Spawn, From,
            State = #state{requests = Reqs, free_nodes = FNs}) ->
    case sets:size(FNs) of
        0 -> {noreply, State#state{requests = queue:in({Spawn, From}, Reqs)}};
        _ -> {noreply, serve(Spawn, From, State)}
    end.

handle_cast({}, _State) ->
    not_implemented.

handle_info({nodedown, N}, State = #state{nodes = Ns, free_nodes = FNs}) ->
    {noreply, State#state{nodes = dict:erase(N, Ns),
                          free_nodes = sets:del_element(N, FNs)}};
handle_info({'DOWN', _, process, Pid, _Reason}, State = #state{free_nodes = FNs,
                                                               nodes = Ns}) ->
    N = node(Pid),
    case dict:find(N, Ns) of
        {ok, NEntry}  ->
            NewNs = dict:store(N, NEntry#node{task_pid = none}, Ns),
            NewFNs = sets:add_element(N, FNs),
            State1 = maybe_serve(State#state{free_nodes = NewFNs,
                                             nodes = NewNs}),
            {noreply, State1};
        error ->
            {noreply, State}
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%------------------------------------------------------------------------------
% Internal
%------------------------------------------------------------------------------

maybe_serve(State = #state{requests = Q}) ->
    case queue:out(Q) of
        {{value, {Spawn, From}}, NewQ} ->
            State1 = serve(Spawn, From, State),
            State1#state{requests = NewQ};
        {empty, _} ->
            State
    end.

serve({spawn, MFA, Link}, From, State = #state{nodes = Ns, free_nodes = FNs}) ->
    N = pick(State),
    Pid = spawn(N, ?MODULE, worker_init, [MFA, Link, self()]),
    M = erlang:monitor(process, Pid),
    receive
        {started, Pid} ->
            gen_server:reply(From, Pid),
            NewNs = dict:update(N, fun (NEntry) ->
                                           NEntry#node{task_pid = Pid}
                                   end, Ns),
            NewFNs = sets:del_element(N, FNs),
            State#state{nodes = NewNs, free_nodes = NewFNs};
        {'DOWN', M, process, Pid, Reason} ->
            case Link of {true, Caller} -> exit(Caller, Reason);
                         false          -> ok
            end
    end.

pick(#state{free_nodes = FNs}) ->
    [N | _] = sets:to_list(FNs),
    N.

worker_init({M, F, A}, Link, Pool) ->
    case Link of {true, Caller} -> link(Caller);
                 false          -> ok
    end,
    Pool ! {started, self()},
    apply(M, F, A).

kill_node(N, State = #state{nodes = Ns, free_nodes = FNs}) ->
    rpc:call(N, init, stop, []),
    receive {nodedown, N} ->
                {killed, State#state{nodes = dict:erase(N, Ns),
                                     free_nodes = sets:del_element(N, FNs)}}
    after 60000 -> monitor_node(N, false),
                   {{error, timed_out_waiting_nodedown}, State}
    end.

quick_kill_node(N, State = #state{nodes = Ns, free_nodes = FNs}) ->
    monitor_node(N, false),
    rpc:cast(N, erlang, halt, []),
    State#state{nodes = dict:erase(N, Ns),
                free_nodes = sets:del_element(N, FNs)}.
