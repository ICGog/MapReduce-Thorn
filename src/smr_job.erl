
-module(smr_job).

-behavior(gen_server).

-export([start_link/5, add_input/2, start/1, task_started/4,
         task_finished/4, next_result/1, kill/1, handover_output/1,
         import_from_output/3]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {sup,
                id,
                phase = input, % input | map | reduce | output
                map_fun,
                reduce_fun,
                awaiting,
                next_awaiting = gb_sets:new(),
                prev_job_import = undefined, % undefined | {OutputTable, From}
                ongoing = 0,
                input_table,
                inter_table,
                output_table,
                cur_task_id = 1,
                input_size = 0,
                available,
                output_buffer = empty,
                output_type = undefined  % undefined | direct
               }).

%------------------------------------------------------------------------------
% Internal API
%------------------------------------------------------------------------------

start_link(MapFun, ReduceFun, JobId, Mode, MaxTasks) ->
    gen_server:start_link(?MODULE, [self(), MapFun, ReduceFun, JobId, Mode,
                                    MaxTasks],
                          []).

add_input(Job, Input) ->
    gen_server:call(Job, {add_input, Input}, infinity).

start(Job) ->
    gen_server:cast(Job, start).

next_result(Job) ->
    gen_server:call(Job, next_result, infinity).

kill(Job) ->
    gen_server:call(Job, kill, infinity).

task_started(Job, Worker, TaskId, Size) ->
    gen_server:call(Job, {task_started, Worker, TaskId, Size}, infinity).

task_finished(Job, Worker, TaskId, Args) ->
    gen_server:cast(Job, {task_finished, Worker, TaskId, Args}).

handover_output(Job) ->
    gen_server:call(Job, handover_output, infinity).

import_from_output(Job, PrevJobId, OutputTable) ->
    gen_server:call(Job, {import_from_output, PrevJobId, OutputTable},
                    infinity).

%------------------------------------------------------------------------------
% Handlers
%------------------------------------------------------------------------------

init([Sup, MapFun, ReduceFun, JobId, Mode, MaxTasks]) ->
        Nodes = smr_pool:get_nodes(),
    NNodes = length(Nodes),
    Available = case MaxTasks of infinity -> NNodes;
                                 _        -> MaxTasks
                end,
    ensure_enough_nodes(Mode, NNodes),
    [InputTable, InterTable, OutputTable] =
        smr_mnesia:create_job_tables(JobId, Nodes, Mode),
    error_logger:info_msg("Job ~p created~n", [JobId]),
    smr_statistics:job_started(JobId, MapFun, ReduceFun),
    {ok, #state{sup = Sup,
                id = JobId,
                map_fun = MapFun,
                reduce_fun = ReduceFun,
                input_table = InputTable,
                inter_table = InterTable,
                output_table = OutputTable,
                available = Available}}.

handle_call({add_input, Input}, _From,
            State = #state{phase = input,
                           input_table = InputTable,
                           cur_task_id = TaskId,
                           input_size = InputSize,
                           next_awaiting = NextAwaiting}) ->
    ChunkSize = smr_mnesia:put_input_chunk(TaskId, Input, InputTable),
    {reply, ok,
     State#state{cur_task_id = TaskId + 1,
                 input_size = InputSize + ChunkSize,
                 next_awaiting = gb_sets:add_element(TaskId, NextAwaiting)}};
handle_call({task_started, WorkerPid, _TaskId, Size}, _From,
            State = #state{phase = Phase, id = JobId}) ->
    erlang:monitor(process, WorkerPid),
    smr_statistics:task_started(JobId, node(WorkerPid), Phase, Size),
    {reply, ok, State};
handle_call(next_result, From, State = #state{output_buffer = empty,
                                              output_type = undefined}) ->
    handle_call(next_result, From,
                buffer_output_chunk(State#state{output_type = direct}));
handle_call(next_result, From, State = #state{output_buffer = Chunk,
                                              output_type = direct,
                                              output_table = OutputTable}) ->
    gen_server:reply(From, Chunk),
    case Chunk of end_of_result -> smr_mnesia:delete_job_table(OutputTable),
                                   {stop, normal, State};
                  _             -> {noreply, buffer_output_chunk(State)}
    end;
handle_call(kill, _From, State) ->
    {stop, killed, ok, State};
handle_call(handover_output, _From,
            State = #state{phase = output,
                           output_type = undefined,
                           output_table = OutputTable}) ->
    {stop, normal, OutputTable, State};
handle_call({import_from_output, PrevJobId, PrevJobOutputTable}, From,
            State = #state{prev_job_import = undefined,
                           id = JobId}) ->
    error_logger:info_msg("Job ~p: importing output from job ~p as input~n",
                          [JobId, PrevJobId]),
    {noreply,
     send_tasks(State#state{prev_job_import = {PrevJobOutputTable, From}})}.

handle_cast(start, State) ->
    {noreply, set_phase(map, State)};
handle_cast({task_finished, Worker, _TaskId, Args},
            State = #state{id = JobId,
                           available = Available,
                           next_awaiting = NextAwaiting,
                           ongoing = Ongoing,
                           phase = Phase,
                           input_size = InputSize}) ->
    smr_statistics:task_finished(JobId, node(Worker)),
    State1 = #state{ongoing = NewOngoing} =
        State#state{available = Available + 1, ongoing = Ongoing - 1},
    MapOrReduceEndFun =
        fun (StateX = #state{awaiting = AwaitingX,
                             ongoing = OngoingX}) ->
            case gb_sets:size(AwaitingX) =:= 0 andalso OngoingX =:= 0 of
                true  -> handle_phase_finished(StateX);
                false -> {noreply, send_tasks(StateX)}
            end
        end,
    case Phase of
        map ->
            [Hashes, TaskResultSize] = Args,
            NewNextAwaiting = gb_sets:union(Hashes,NextAwaiting),
            MapOrReduceEndFun(State1#state{input_size = InputSize +
                                                           TaskResultSize,
                                           next_awaiting = NewNextAwaiting});
        reduce ->
            [] = Args,
            MapOrReduceEndFun(State1);
        input ->
            case Args of
                [TaskId1, TaskResultSize] ->
                    NewNextAwaiting = gb_sets:add_element(TaskId1,
                                                          NextAwaiting),
                    {noreply,
                     send_tasks(State1#state{input_size = InputSize +
                                                              TaskResultSize,
                                             next_awaiting = NewNextAwaiting})};
                end_of_result ->
                    case NewOngoing of 0 -> handle_phase_finished(State1);
                                       _ -> {noreply, State1}
                    end

            end
    end.

handle_info({'DOWN', _, process, Pid, Reason}, State = #state{id = JobId}) ->
    case Reason of
        normal -> {noreply, State};
        _      -> smr_statistics:task_failed(JobId, node(Pid)),
                  {noreply, State}
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%------------------------------------------------------------------------------
% Internal
%------------------------------------------------------------------------------

handle_phase_finished(
        State = #state{phase = input,
                       prev_job_import = {PrevJobOutputTable, From}}) ->
    gen_server:reply(From, ok),
    spawn_link(fun () -> smr_mnesia:delete_job_table(PrevJobOutputTable) end),
    {noreply, State#state{prev_job_import = undefined}};
handle_phase_finished(State = #state{phase = map, input_table = InputTable}) ->
    spawn_link(fun () -> smr_mnesia:delete_job_table(InputTable) end),
    {noreply, set_phase(reduce, State)};
handle_phase_finished(State = #state{phase = reduce,
                                     id = JobId,
                                     inter_table = InterTable}) ->
    spawn_link(fun () -> smr_mnesia:delete_job_table(InterTable) end),
    smr_statistics:job_finished(JobId),
    smr_master:job_finished(self()),
    {noreply, set_phase(output, State)}.

set_phase(map, State = #state{input_size = InputSize,
                              id = JobId,
                              next_awaiting = NextAwaiting}) ->
    error_logger:info_msg("Job ~p: map phase started~n", [JobId]),
    smr_statistics:job_next_phase(JobId, map, InputSize),
    send_tasks(State#state{phase = map,
                           input_size = 0,
                           awaiting = NextAwaiting,
                           next_awaiting = gb_sets:new()});
set_phase(reduce, State = #state{input_size = InputSize,
                                 id = JobId,
                                 next_awaiting = Awaiting}) ->
    error_logger:info_msg("Job ~p: reduce phase started~n", [JobId]),
    smr_statistics:job_next_phase(JobId, reduce, InputSize),
    send_tasks(State#state{phase = reduce,
                           awaiting = Awaiting,
                           next_awaiting = none});
set_phase(output, State = #state{id = JobId}) ->
    error_logger:info_msg("Job ~p: output phase started~n", [JobId]),
    State#state{phase = output}.

send_tasks(State = #state{available = 0}) ->
    State;
send_tasks(State = #state{sup = Sup,
                          phase = Phase,
                          map_fun = MapFun,
                          reduce_fun = ReduceFun,
                          ongoing = Ongoing,
                          awaiting = Awaiting,
                          available = Available,
                          cur_task_id = CurTaskId,
                          input_table = InputTable,
                          inter_table = InterTable,
                          output_table = OutputTable,
                          prev_job_import = PrevJobImport}) ->
    {Fun, FromTable, ToTable} =
        case Phase of
            map    -> {MapFun, InputTable, InterTable};
            reduce -> {ReduceFun, InterTable, OutputTable};
            input  -> {PrevJobOutputTable, _} = PrevJobImport,
                      {none, PrevJobOutputTable, InputTable}
        end,
    case Awaiting =/= undefined andalso gb_sets:size(Awaiting) =:= 0 andalso
             (Phase =:= map orelse Phase =:= reduce) of
        true ->
            State;
        false ->
            {TaskId, State1} =
                case Phase of
                    input ->
                        {CurTaskId, State#state{cur_task_id = CurTaskId + 1}};
                    _MapOrReduce ->
                        {TaskId1, NewAwaiting} =
                            gb_sets:take_smallest(Awaiting),
                        {TaskId1, State#state{awaiting = NewAwaiting}}
                end,
            JobPid = self(),
            spawn_link(
                fun () ->
                        case smr_task_sup_sup:start_task(
                                 smr_job_sup:task_sup_sup(Sup), JobPid, TaskId,
                                 Phase, Fun, FromTable, ToTable) of
                            {ok, _}         -> ok;
                            {error, Reason} -> exit(Reason)
                        end
                end),
            send_tasks(State1#state{available = Available - 1,
                                    ongoing = Ongoing + 1})
    end.

buffer_output_chunk(State = #state{output_table = OutputTable}) ->
    Chunk = case smr_mnesia:take_output_chunk(OutputTable) of
                {C, _}        -> C;
                end_of_result -> end_of_result
            end,
    State#state{output_buffer = Chunk}.

ensure_enough_nodes(Mode, NNodes) ->
    Req = lists:foldl(fun ({_, Replicas}, Sum) -> Sum + Replicas end, 0, Mode),
    if NNodes < Req -> exit({not_enough_nodes_for_mode,
                             {req, Req}, {num_nodes, NNodes}});
       true         -> ok
    end.
