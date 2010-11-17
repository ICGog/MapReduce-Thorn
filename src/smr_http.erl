
-module(smr_http).

-export([start/0, hello_world/3]).

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
        {port,8081},
        {server_name, "smr"},
        {server_root, "log"},
        {document_root, "www"},
        {erl_script_alias, {"/smr", [?MODULE]}},
        {error_log, "error.log"},
        {security_log, "security.log"},
        {transfer_log, "transfer.log"},
        {mime_types, [{"html", "text/html"},
                      {"css", "text/css"},
                      {"js", "application/x-javascript"}]}]).

hello_world(SessionId, _Env, _Input) ->
    mod_esi:deliver(
        SessionId,
        ["Content-Type: text/html\r\n\r\n",
         "<html><body>Hello, World!</body></html>"
        ]).
