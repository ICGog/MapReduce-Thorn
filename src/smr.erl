
-module(smr).

-behaviour(application).

-export([start/0, stop/0, auto_attach_worker_nodes/0, attach_worker_node/1,
         kill_worker_node/1, get_worker_nodes/0, master/0, statistics/0]).
-export([start/2, stop/1]).

%%---------------------------------------------------------------------------

start() ->
    application:start(smr).

stop() ->
    application:stop(smr).

auto_attach_worker_nodes() ->
    Workers = lists:map(fun (Worker) -> list_to_atom(
                                            case lists:member($@, Worker) of
                                                true  -> Worker;
                                                false -> Worker ++ "@" ++
                                                             net_adm:localhost()
                                            end)
                        end, string:tokens(os:getenv("SMR_WORKER_NODES"),
                                           " \n\t")),
    Result = lists:map(fun attach_worker_node/1, Workers),
    lists:zip(Workers, Result).

attach_worker_node(Node) ->
    case pool:attach(Node) of
        attached -> smr_statistics:register_worker(statistics(), Node),
                    attached;
        Other    -> Other
    end.

kill_worker_node(Node) ->
    case {lists:member(Node, pool:get_nodes()), node()} of
        {_, Node}  -> {error, master_node};
        {false, _} -> {error, not_worker_node};
        {true, _}  -> monitor_node(Node, true),
                      rpc:call(Node, init, stop, []),
                      receive {nodedown, Node} -> killed
                      after 60000 -> {error, timed_out_waiting_nodedown}
                      end
    end.

get_worker_nodes() ->
    pool:get_nodes().

master() ->
    global:whereis_name(smr_master).

statistics() ->
    global:whereis_name(smr_statistics).

%%---------------------------------------------------------------------------

start(_StartType, [EnableWebsite]) ->
    Nodes = pool:start(smr_worker),
    {ok, Sup} = smr_sup:start_link(Nodes),
    case EnableWebsite of true  -> smr_http:start();
                          false -> ok
    end,
    {ok, Sup}.

stop(_State) ->
    pool:stop(),
    ok.
