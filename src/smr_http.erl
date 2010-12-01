
-module(smr_http).

-export([start/0, get_all_workers/3, get_workers/3, kill_worker/3]).

-include("smr.hrl").

start() ->
    inets:start(),
    inets:start(httpd, [
        {modules, [
           mod_alias,
           mod_auth,
           mod_esi,
           mod_actions,
           mod_cgi,
           mod_dir,
           mod_get,
           mod_head,
           mod_log,
           mod_disk_log]},
        {port, 8081},
        {server_name, "smr"},
        {server_root, "log"},
        {document_root, "www"},
        {erl_script_alias, {"/smr", [?MODULE]}},
        {error_log, "error.log"},
        {security_log, "security.log"},
        {transfer_log, "transfer.log"},
        {mime_types, [{"html", "text/html"},
                      {"css", "text/css"},
                      {"js", "application/x-javascript"},
                      {"png", "image/png"}]}]).

rfc4627_header() ->
    "Content-Type: " ++ rfc4627:mime_type() ++ "\r\n\r\n".

get_all_workers(SessionId, _Env, _Input) ->
    Workers = lists:map(fun erlang:atom_to_list/1,
                        smr_statistics:get_all_workers(smr:statistics())),
    mod_esi:deliver(SessionId, [rfc4627_header(), rfc4627:encode(Workers)]).

get_workers(SessionId, _Env, _Input) ->
    Workers = lists:map(fun({_, Value}) -> worker_to_json_spec(Value) end,
                        smr_statistics:get_workers(smr:statistics())),
    mod_esi:deliver(SessionId, 
                    [rfc4627_header(), rfc4627:encode(Workers)]).

kill_worker(SessionId, Env, _Input) ->
    {query_string, Query} = lists:keyfind(query_string, 1, Env),
    [{"w", Node}] = lists:filter(fun({Key, _Value}) -> Key == "w" end, 
                                 httpd:parse_query(Query)),
    case smr:kill_worker_node(list_to_atom(Node)) of
        killed     -> mod_esi:deliver(SessionId, 
                        [rfc4627_header(),
                         rfc4627:encode({obj, [{result, <<"ok">>}]})]);
        {error, _} -> mod_esi:deliver(SessionId,
                        [rfc4627_header(), 
                         rfc4627:encode({obj, [{result, <<"error">>}]})])
    end.

%------------------------------------------------------------------------------
% Internal
%------------------------------------------------------------------------------

worker_to_json_spec(Worker) ->
    record_to_json_spec(Worker, record_info(fields, worker)).

record_to_json_spec(Record, Fields) ->
     {obj, record_to_map(Record, Fields)}.

record_to_map(Record, Fields) ->
    [_ | Values] = tuple_to_list(Record),
    lists:zip(Fields, Values).
