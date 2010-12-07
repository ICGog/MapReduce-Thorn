
-module(smr_http).

-export([start/0, get_workers/3, get_jobs/3, kill_worker/3]).

-include("smr.hrl").

-record(date_time, {year, month, day, hour, minute, second}).

start() ->
    inets:start(),
    error_logger:info_msg("~nMonitoring URL: http://" ++ net_adm:localhost() ++
                          ":" ++ integer_to_list(?WWW_PORT) ++
                          "/index.html~n~n", []),
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

get_workers(SessionId, _Env, _Input) ->
    Workers =
        lists:map(
            fun worker_to_json_spec/1,
            lists:filter(fun (#smr_worker{is_detached = Det}) -> not Det end,
                         snd(lists:unzip(smr_statistics:get_workers())))),
    mod_esi:deliver(SessionId, [rfc4627_header(), rfc4627:encode(Workers)]).

get_jobs(SessionId, _Env, _Input) ->
    Jobs = lists:map(fun job_to_json_spec/1,
                     snd(lists:unzip(smr_statistics:get_jobs()))),
    mod_esi:deliver(SessionId, [rfc4627_header(), rfc4627:encode(Jobs)]).

kill_worker(SessionId, Env, _Input) ->
    {query_string, Query} = lists:keyfind(query_string, 1, Env),
    [{"w", Node}] = lists:filter(fun({Key, _Value}) -> Key == "w" end, 
                                 httpd:parse_query(Query)),
    case smr_pool:kill_node(list_to_atom(Node)) of
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

job_to_json_spec(Job = #smr_job{started_on = SO, ended_on = EO}) ->
    Json_SO = now_to_json_spec(SO),
    Json_EO = now_to_json_spec(EO),
    record_to_json_spec(Job#smr_job{started_on = Json_SO,
                                    ended_on = Json_EO},
                        record_info(fields, smr_job)).

worker_to_json_spec(Worker = #smr_worker{last_task_started_on = LTSO}) ->
    record_to_json_spec(
        Worker#smr_worker{last_task_started_on = now_to_json_spec(LTSO)},
        record_info(fields, smr_worker)).

now_to_json_spec(undefined) ->
    <<"undefined">>;
now_to_json_spec(Now) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:now_to_local_time(Now),
    DT = #date_time{year = Year, month = Month, day = Day, hour = Hour,
                    minute = Minute, second = Second},
    record_to_json_spec(DT, record_info(fields, date_time)).

record_to_json_spec(Record, Fields) ->
     {obj, record_to_map(Record, Fields)}.

record_to_map(Record, Fields) ->
    [_ | Values] = tuple_to_list(Record),
    lists:zip(Fields, Values).

snd({_, Snd}) -> Snd.
