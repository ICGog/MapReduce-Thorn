
-module(smr_mnesia).

-export([start/0, stop/0, start_on_node/1, stop_on_node/1]).
-export([create_job_tables/3, delete_job_table/1, delete_job_tables/1]).
-export([put_input_chunk/3, process_input/4, process_inter/4,
         take_output_chunk/1]).

-include("smr.hrl").

-record(smr_input, {task_id, chunk}).

-record(smr_inter_k, {hash_key, key, uuid}).
-record(smr_inter, {k, value}).

-record(smr_output, {key, value}).

%------------------------------------------------------------------------------
% Internal API
%------------------------------------------------------------------------------

start() ->
    ok = mnesia:delete_schema([node()|nodes()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start().

stop() ->
    ok = mnesia:delete_schema([node()]),
    stopped = mnesia:stop().

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
create_job_tables(JobId, Ns, Mode) ->
    Common = [{frag_properties, [{node_pool, Ns}, {n_fragments, length(Ns)}] ++
                                Mode}],
    lists:map(
        fun ({Which, TableDef}) ->
                TableName = table(Which, JobId),
                {atomic, ok} = mnesia:create_table(TableName, TableDef),
                TableName
        end,
        [{input, [{record_name, smr_input},
                  {attributes, record_info(fields, smr_input)}] ++ Common},
         {inter, [{record_name, smr_inter},
                  {attributes, record_info(fields, smr_inter)},
                  {type, ordered_set}] ++ Common},
         {output, [{record_name, smr_output},
                   {attributes, record_info(fields, smr_output)}] ++ Common}]).

delete_job_table(Table) ->
    mnesia:delete_table(Table).

delete_job_tables(JobId) ->
    lists:foreach(fun (Which) -> mnesia:delete_table(table(Which, JobId)) end,
                  [input, inter, output]).

put_input_chunk(TaskId, Chunk, InputTable) ->
    mnesia:activity(sync_dirty, fun put_input_chunk_internal/3,
                    [TaskId, Chunk, InputTable], mnesia_frag).

process_input(TaskId, Fun, InputTable, InterTable) ->
    mnesia:activity(sync_dirty, fun process_input_internal/4,
                    [TaskId, Fun, InputTable, InterTable], mnesia_frag).

process_inter(HashKey, Fun, InterTable, OutputTable) ->
    mnesia:activity(sync_dirty, fun process_inter_internal/4,
                    [HashKey, Fun, InterTable, OutputTable], mnesia_frag).

take_output_chunk(OutputTable) ->
    mnesia:activity(sync_dirty, fun take_output_chunk_internal/1,
                    [OutputTable], mnesia_frag).

%------------------------------------------------------------------------------
% Internal
%------------------------------------------------------------------------------

put_input_chunk_internal(TaskId, Chunk, InputTable) ->
    mnesia:write(InputTable, #smr_input{task_id = TaskId, chunk = Chunk},
                 write),
    ok.

process_input_internal(TaskId, Fun, InputTable, InterTable) ->
    [#smr_input{chunk = Chunk}] = mnesia:read(InputTable, TaskId, read),
    {Hashes, ResultSize} =
        lists:foldl(
            fun (KV = {Key, Value}, {HashesSet, TotalSize}) ->
                    Hash = erlang:phash2(Key),
                    mnesia:write(InterTable,
                                 #smr_inter{k = #smr_inter_k{hash_key = Hash,
                                                             key = Key,
                                                             uuid = uuid()},
                                            value = Value},
                                write),
                   {gb_sets:add_element(Hash, HashesSet),
                    TotalSize + erts_debug:flat_size(KV)}
            end, {gb_sets:new(), 0}, Fun(Chunk)),
    {Hashes, ResultSize}.

process_inter_internal(HashKey, Fun, InterTable, OutputTable) ->
    KVList = mnesia:select(InterTable,
                           [{#smr_inter{k = #smr_inter_k{hash_key = HashKey,
                                                         key = '$1',
                                                         _='_'},
                                        value = '$2',
                                        _='_'},
                            [], [{{'$1', '$2'}}]}],
                           read),
    Dict =
        lists:foldl(
            fun ({K, V}, Dict) ->
                    orddict:update(K, fun (VList) -> [V | VList] end, [V], Dict)
            end, orddict:new(), KVList),
    orddict:fold(
         fun (Key, Values, _) ->
                 ReducedValue = Fun({Key, Values}),
                 mnesia:write(OutputTable, #smr_output{key = Key,
                                                       value = ReducedValue},
                              write)
         end, none, Dict).

take_output_chunk_internal(OutputTable) ->
    case select_until_size(mnesia:select(OutputTable,
                                         [{#smr_output{_='_'}, [], ['$_']}],
                                         10, read)) of
        '$end_of_table' ->
            end_of_result;
        Rs ->
            lists:map(fun (R = #smr_output{key = K, value = V}) ->
                              mnesia:delete_object(OutputTable, R, write),
                              {K, V}
                      end, Rs)
    end.

select_one_while_empty('$end_of_table') ->
    '$end_of_table';
select_one_while_empty({[], Cont}) ->
    select_one_while_empty(mnesia:select(Cont));
select_one_while_empty({[Obj | _Rest], _Cont}) ->
    Obj.

select_until_size(Select) ->
    select_until_size2(Select, ?CHUNK_SIZE, []).

select_until_size2('$end_of_table', _RemSize, []) ->
    '$end_of_table';
select_until_size2('$end_of_table', _RemSize, Acc) ->
    Acc;
select_until_size2({List, Cont}, RemSize, Acc) ->
    NewRemSize = RemSize - erts_debug:flat_size(List),
    if NewRemSize > 0 -> select_until_size2(mnesia:select(Cont), NewRemSize,
                                            List ++ Acc);
       true           -> List ++ Acc
    end.

table(input, JobId)  -> list_to_atom("smr_input_"  ++ integer_to_list(JobId));
table(inter, JobId)  -> list_to_atom("smr_inter_"  ++ integer_to_list(JobId));
table(output, JobId) -> list_to_atom("smr_output_" ++ integer_to_list(JobId)).

uuid() ->
    {node(), now()}.
