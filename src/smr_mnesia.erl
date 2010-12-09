
-module(smr_mnesia).

-export([start/0, stop/0, start_on_node/1, stop_on_node/1]).
-export([create_job_table/2, delete_job_table/1, tables_monitor/1]).
-export([put_input_chunk/2, process_input/4, process_inter/4,
         get_output_chunk/2]).

-include("smr.hrl").

-record(smr_data, {k, % {hash, uuid}
                   chunk}).

%------------------------------------------------------------------------------
% Internal API
%------------------------------------------------------------------------------

start() ->
    ok = mnesia:delete_schema([node()|nodes()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start().

stop() ->
    stopped = mnesia:stop(),
    ok = mnesia:delete_schema([node()]).

start_on_node(Node) ->
    ok = rpc:call(Node, mnesia, delete_schema, [[Node]]),
    ok = rpc:call(Node, mnesia, start, []),
    mnesia:change_config(extra_db_nodes, [Node]),
    mnesia:change_table_copy_type(schema, Node, disc_copies),
    ok.

stop_on_node(Node) ->
    stopped = rpc:call(Node, mnesia, stop, []),
    ok = rpc:call(Node, mnesia, delete_schema, [[Node]]).

%% Mode = [{n_ram_copies | n_disc_copies | n_disc_only_copies,
%%          NumberOfReplicas}]
create_job_table(Ns, Mode) ->
    TableName = uuid_table_name(),
    mnesia:create_table(
        TableName,
        [{record_name, smr_data},
         {attributes, record_info(fields, smr_data)},
         {type, ordered_set},
         {frag_properties, [{node_pool, Ns}, {n_fragments, length(Ns)}] ++
                            Mode}]),
    TableName.

delete_job_table(Table) ->
    mnesia:delete_table(Table).

tables_monitor(Tables) ->
    spawn_link(
        fun () ->
                process_flag(trap_exit, true),
                receive
                    {'EXIT', _, normal} ->
                        ok;
                    {'EXIT', _, _Reason} ->
                        lists:foreach(fun delete_job_table/1, Tables)
                end
        end).

put_input_chunk(Chunk, InputTable) ->
    mnesia:activity(sync_dirty, fun put_input_chunk_internal/2,
                    [Chunk, InputTable], mnesia_frag).

process_input(LookupHash, Fun, InputTable, InterTable) ->
    mnesia:activity(sync_dirty, fun process_input_internal/4,
                    [LookupHash, Fun, InputTable, InterTable], mnesia_frag).

process_inter(LookupHash, Fun, InterTable, OutputTable) ->
    mnesia:activity(sync_dirty, fun process_inter_internal/4,
                    [LookupHash, Fun, InterTable, OutputTable], mnesia_frag).

get_output_chunk(LookupHash, OutputTable) ->
    mnesia:activity(sync_dirty, fun get_output_chunk_internal/2,
                    [LookupHash, OutputTable], mnesia_frag).

%------------------------------------------------------------------------------
% Internal
%------------------------------------------------------------------------------

put_input_chunk_internal(Chunk, InputTable) ->
    {Hash, K} = new_k(),
    write_chunk(K, Chunk, InputTable),
    {[Hash], erts_debug:flat_size(Chunk)}.

process_input_internal(LookupHash, Fun, InputTable, InterTable) ->
    Chunks = lists:flatten(select_chunks(LookupHash, InputTable)),
    NewChunks = aggregate(Fun(Chunks)),
    Size = erts_debug:flat_size(NewChunks),
    %HashFun = chunk_collide_hash_fun(Size),
    HashFun = fun erlang:phash2/1,
    HashesSet =
        lists:foldl(
            fun (Chunk = {Key, _Values}, HashesSet) ->
                    {Hash, K} = new_k(HashFun, Key),
                    write_chunk(K, Chunk, InterTable),
                    ordsets:add_element(Hash, HashesSet)
            end, ordsets:new(), NewChunks),
    {ordsets:to_list(HashesSet), Size}.

process_inter_internal(LookupHash, Fun, InterTable, OutputTable) ->
    Chunk = reaggregate(select_chunks(LookupHash, InterTable)),
    NewChunk = Fun(Chunk),
    Size = erts_debug:flat_size(NewChunk),
    {Hash, K} = new_k(chunk_collide_hash_fun(Size)),
    write_chunk(K, NewChunk, OutputTable),
    {[Hash], Size}.

get_output_chunk_internal(LookupHash, OutputTable) ->
    lists:flatten(select_chunks(LookupHash, OutputTable)).

write_chunk(K, Chunk, Table) ->
    mnesia:write(Table, #smr_data{k = K, chunk = Chunk}, write).

select_chunks(LookupHash, Table) ->
    mnesia:select(Table, [{#smr_data{k = {LookupHash, '_'},
                                     chunk = '$1',
                                     _='_'},
                           [], ['$1']}],
                  read).

aggregate(KVList) ->
    orddict:to_list(
        lists:foldl(
            fun ({K, V}, Dict) ->
                    orddict:update(K, fun (VList) -> [V | VList] end, [V], Dict)
            end, orddict:new(), KVList)).

reaggregate(KVsList) ->
    orddict:to_list(
        lists:foldl(
            fun ({K, Vs}, Dict) ->
                    orddict:update(K, fun (VList) -> Vs ++ VList end, Vs, Dict)
            end, orddict:new(), KVsList)).

uuid_table_name() ->
    {A, B, C} = now(),
    list_to_atom("smr_" ++ integer_to_list(A) ++ "_" ++
        integer_to_list(B) ++ "_" ++ integer_to_list(C)).

uuid() ->
    {A, B, C} = now(),
    {node(), A, B, C}.

new_k() ->
    new_k(fun erlang:phash2/1).

new_k(HashFun) ->
    ID = term_to_binary(uuid()),
    Hash = HashFun(ID),
    {Hash, {Hash, ID}}.

new_k(HashFun, HashFunArg) ->
    ID = term_to_binary(uuid()),
    Hash = HashFun(HashFunArg),
    {Hash, {Hash, ID}}.

chunk_collide_hash_fun(Size) when ?CHUNK_SIZE > Size ->
    fun (Arg) -> erlang:phash2(Arg, ?CHUNK_SIZE div Size) end;
chunk_collide_hash_fun(_) ->
    fun erlang:phash2/1.
