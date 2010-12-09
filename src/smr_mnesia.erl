
-module(smr_mnesia).

-export([start/0, stop/0, start_on_node/1, stop_on_node/1]).
-export([create_job_tables/3, delete_job_table/1, delete_job_tables/1]).
-export([put_input_chunk/3, migrate_output_to_input/3, process_input/5,
         process_inter/4, take_output_chunk/1]).

-include("smr.hrl").

-record(smr_input, {task_id, chunk}).

-record(smr_inter_k, {hash_key, key, uuid}).
-record(smr_inter, {k, values}).

-record(smr_output, {key, value}).

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
    mnesia:activity(sync_dirty, fun put_input_chunk_internal_size/3,
                    [TaskId, Chunk, InputTable], mnesia_frag).

migrate_output_to_input(TaskId, OutputTable, InputTable) ->
    mnesia:activity(sync_transaction, fun migrate_output_to_input_internal/3,
                    [TaskId, OutputTable, InputTable], mnesia_frag).

process_input(TaskId, Fun, HashFun, InputTable, InterTable) ->
    mnesia:activity(sync_dirty, fun process_input_internal/5,
                    [TaskId, Fun, HashFun, InputTable, InterTable],
                    mnesia_frag).

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

put_input_chunk_internal_size(TaskId, Chunk, InputTable) ->
    mnesia:write(InputTable, #smr_input{task_id = TaskId, chunk = Chunk},
                 write),
    erts_debug:flat_size(Chunk).

migrate_output_to_input_internal(TaskId, OutputTable, InputTable) ->
    case take_output_chunk_internal(OutputTable) of
        end_of_result -> end_of_result;
        {Chunk, Size} -> put_input_chunk_internal(TaskId, Chunk, InputTable),
                         Size
    end.

process_input_internal(TaskId, Fun, HashFun, InputTable, InterTable) ->
    [#smr_input{chunk = Chunk}] = mnesia:read(InputTable, TaskId, read),
    lists:foldl(
        fun ({Key, Values}, {HashesSet, TotalSize}) ->
                Hash = HashFun(Key),
                mnesia:write(InterTable,
                             #smr_inter{k = #smr_inter_k{hash_key = Hash,
                                                         key = Key,
                                                         uuid = uuid()},
                                        values = Values},
                            write),
               {gb_sets:add_element(Hash, HashesSet),
                TotalSize + erts_debug:flat_size(Key) +
                    erts_debug:flat_size(Values)}
        end, {gb_sets:new(), 0}, aggregate(Fun(Chunk))).

process_inter_internal(HashKey, Fun, InterTable, OutputTable) ->
    KVsList = mnesia:select(InterTable,
                            [{#smr_inter{k = #smr_inter_k{hash_key = HashKey,
                                                          key = '$1',
                                                          _='_'},
                                         values = '$2',
                                         _='_'},
                             [], [{{'$1', '$2'}}]}],
                            read),
    lists:foreach(
        fun ({K, V}) ->
                mnesia:write(OutputTable, #smr_output{key = K, value = V},
                             write)
        end, Fun(reaggregate(KVsList))).

take_output_chunk_internal(OutputTable) ->
    case select_until_size(mnesia:select(OutputTable,
                                         [{#smr_output{_='_'}, [], ['$_']}],
                                         10, write)) of
        '$end_of_table' ->
            end_of_result;
        {Rs, Size} ->
            {lists:map(fun (R = #smr_output{key = K, value = V}) ->
                               mnesia:delete_object(OutputTable, R, write),
                               {K, V}
                       end, Rs),
             Size}
    end.

select_until_size(Select) ->
    case select_until_size2(Select, ?CHUNK_SIZE, []) of
        {Chunk, RemSize} -> {Chunk, ?CHUNK_SIZE - RemSize};
        '$end_of_table'  -> '$end_of_table'
    end.

select_until_size2('$end_of_table', _RemSize, []) ->
    '$end_of_table';
select_until_size2('$end_of_table', RemSize, Acc) ->
    {Acc, RemSize};
select_until_size2({List, Cont}, RemSize, Acc) ->
    NewRemSize = RemSize - erts_debug:flat_size(List),
    if NewRemSize > 0 -> select_until_size2(mnesia:select(Cont), NewRemSize,
                                            List ++ Acc);
       true           -> {List ++ Acc, NewRemSize}
    end.

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

table(input, JobId)  -> list_to_atom("smr_input_"  ++ integer_to_list(JobId));
table(inter, JobId)  -> list_to_atom("smr_inter_"  ++ integer_to_list(JobId));
table(output, JobId) -> list_to_atom("smr_output_" ++ integer_to_list(JobId)).

uuid() ->
    {node(), now()}.
