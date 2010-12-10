
-module(smr_job).

-behavior(gen_server).

-export([start_link/5, add_input/2, start/1, task_started/4,
         task_finished/5, next_result/1, kill/1, handover_output/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2,
         code_change/3]).

-include("smr.hrl").

-record(state, {sup,
                id,
                phase = input, % input | map | reduce | output
                map_fun,
                reduce_fun,
                awaiting,
                next_awaiting,
                ongoing = 0,
                input_table,
                inter_table,
                output_table,
                size,
                next_size,
                available,
                output_buffer = empty,
                props}).

%------------------------------------------------------------------------------
% Internal API
%------------------------------------------------------------------------------

start_link(InputMode, MapFun, ReduceFun, JobId, Props) ->
    gen_server:start_link(?MODULE, [self(), InputMode, MapFun, ReduceFun,
                                    JobId, Props], []).

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

task_finished(Job, Worker, TaskId, NewHashes, ResultSize) ->
    gen_server:cast(Job,
                    {task_finished, Worker, TaskId, NewHashes, ResultSize}).

handover_output(Job) ->
    gen_server:call(Job, handover_output, infinity).

%------------------------------------------------------------------------------
% Handlers
%------------------------------------------------------------------------------

init([Sup, InputMode, MapFun, ReduceFun, JobId, Props]) ->
    Nodes = smr_pool:get_nodes(),
    NNodes = length(Nodes),
    Available = proplists:get_value(max_tasks, Props, NNodes),
    ReplicationMode = proplists:get_value(replication_mode, Props,
                                          ?REPLICATION_MODE),
    ensure_enough_nodes(ReplicationMode, NNodes),
    {NextAwaiting, NextSize} =
        case InputMode of
            direct                           -> {gb_sets:new(), 0};
            {other_job, {Awaiting, Size, _}} -> {Awaiting, Size}
        end,
    InputTable = case InputMode of
                     direct ->
                         smr_mnesia:create_job_table(Nodes, ReplicationMode);
                     {other_job, {_, _, OtherJobTable}} ->
                         OtherJobTable
                 end,
    OutputTable = smr_mnesia:create_job_table(Nodes, ReplicationMode),
    InterTable =
        case ReduceFun of
            none -> smr_mnesia:tables_monitor([InputTable, OutputTable]),
                    none;
            _    -> IT = smr_mnesia:create_job_table(Nodes, ReplicationMode),
                    smr_mnesia:tables_monitor([InputTable, IT, OutputTable]),
                    IT
        end,
    error_logger:info_msg("Job ~p created~n", [JobId]),
    smr_statistics:job_started(JobId, MapFun, ReduceFun),
    {ok, #state{sup = Sup,
                id = JobId,
                props = Props,
                map_fun = MapFun,
                next_size = NextSize,
                next_awaiting = NextAwaiting,
                reduce_fun = ReduceFun,
                input_table = InputTable,
                inter_table = InterTable,
                output_table = OutputTable,
                available = Available}}.

handle_call({add_input, Input}, _From,
            State = #state{phase = input,
                           input_table = InputTable,
                           props = Props}) ->
    {reply, ok,
     add_task_operation_outcome(smr_mnesia:put_input_chunk(Input, InputTable,
                                                           Props),
                                State)};
handle_call({task_started, WorkerPid, _TaskId, Size}, _From,
            State = #state{phase = Phase, id = JobId}) ->
    erlang:monitor(process, WorkerPid),
    smr_statistics:task_started(JobId, node(WorkerPid), Phase, Size),
    {reply, ok, State};
handle_call(next_result, From, State = #state{output_buffer = empty}) ->
    handle_call(next_result, From, buffer_output_chunk(State));
handle_call(next_result, From, State = #state{output_buffer = Chunk,
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
                           awaiting = Awaiting,
                           size = Size,
                           output_table = OutputTable}) ->
    {stop, normal, {Awaiting, Size, OutputTable}, State}.

handle_cast(start, State) ->
    {noreply, new_phase(State#state{phase = map})};
handle_cast({task_finished, Worker, _TaskId, NewHashes, TaskResultSize},
            State = #state{id = JobId,
                           available = Available,
                           ongoing = Ongoing,
                           awaiting = Awaiting}) ->
    smr_statistics:task_finished(JobId, node(Worker)),
    NewOngoing = Ongoing - 1,
    State1 = add_task_operation_outcome({NewHashes, TaskResultSize}, State),
    State2 = State1#state{available = Available + 1, ongoing = NewOngoing},
    case {gb_sets:size(Awaiting), NewOngoing} of
        {0, 0} -> handle_phase_finished(State2);
        _      -> {noreply, send_tasks(State2)}
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

add_task_operation_outcome({NewHashes, TaskResultSize},
                           State = #state{next_awaiting = NextAwaiting,
                                          next_size = NextSize}) ->
    NewNextAwaiting = gb_sets:union(gb_sets:from_ordset(NewHashes),
                                    NextAwaiting),
    State#state{next_awaiting = NewNextAwaiting,
                next_size = NextSize + TaskResultSize}.

handle_phase_finished(State = #state{phase = map,
                                     reduce_fun = none,
                                     id = JobId,
                                     input_table = InputTable}) ->
    spawn_link(fun () -> smr_mnesia:delete_job_table(InputTable) end),
    smr_statistics:job_finished(JobId),
    smr_master:job_finished(self()),
    {noreply, new_phase(State#state{phase = output})};
handle_phase_finished(State = #state{phase = map, input_table = InputTable}) ->
    spawn_link(fun () -> smr_mnesia:delete_job_table(InputTable) end),
    {noreply, new_phase(State#state{phase = reduce})};
handle_phase_finished(State = #state{phase = reduce,
                                     id = JobId,
                                     inter_table = InterTable}) ->
    spawn_link(fun () -> smr_mnesia:delete_job_table(InterTable) end),
    smr_statistics:job_finished(JobId),
    smr_master:job_finished(self()),
    {noreply, new_phase(State#state{phase = output})}.

new_phase(State = #state{phase = NewPhase,
                         next_size = Size,
                         id = JobId,
                         next_awaiting = NextAwaiting}) ->
    error_logger:info_msg("Job ~p: ~p phase started~n", [JobId, NewPhase]),
    State1 = State#state{next_size = 0,
                         size = Size,
                         awaiting = NextAwaiting,
                         next_awaiting = gb_sets:new()},
    case NewPhase of
        output       -> State1;
        _MapOrReduce -> smr_statistics:job_next_phase(JobId, NewPhase, Size),
                        send_tasks(State1)
    end.

send_tasks(State = #state{available = 0}) ->
    State;
send_tasks(State = #state{sup = Sup,
                          phase = Phase,
                          map_fun = MapFun,
                          reduce_fun = ReduceFun,
                          props = Props,
                          ongoing = Ongoing,
                          awaiting = Awaiting,
                          available = Available,
                          input_table = InputTable,
                          inter_table = InterTable,
                          output_table = OutputTable}) ->
    {TaskType, Fun, FromTable, ToTable} =
        case {Phase, ReduceFun} of
            {map, none}    -> {map_no_reduce, MapFun, InputTable, OutputTable};
            {map, _}       -> {map, MapFun, InputTable, InterTable};
            {reduce, _}    -> {reduce, ReduceFun, InterTable, OutputTable}
        end,
    case gb_sets:size(Awaiting) of
        0 ->
            State;
        _ ->
            {LookupHash, NewAwaiting} = gb_sets:take_smallest(Awaiting),
            JobPid = self(),
            spawn_link(
                fun () ->
                        case smr_task_sup_sup:start_task(
                                 smr_job_sup:task_sup_sup(Sup), TaskType,
                                 JobPid, LookupHash, Fun, FromTable, ToTable,
                                 Props) of
                            {ok, _}         -> ok;
                            {error, Reason} -> exit(Reason)
                        end
                end),
            send_tasks(State#state{available = Available - 1,
                                   ongoing = Ongoing + 1,
                                   awaiting = NewAwaiting})
    end.

buffer_output_chunk(State = #state{output_table = OutputTable,
                                   awaiting = Awaiting,
                                   props = Props}) ->
    case gb_sets:size(Awaiting) of
        0 -> State#state{output_buffer = end_of_result};
        _ -> {LookupHash, NewAwaiting} = gb_sets:take_smallest(Awaiting),
             Chunk = smr_mnesia:get_output_chunk(LookupHash, OutputTable,
                                                 Props),
             State#state{output_buffer = Chunk,
                         awaiting = NewAwaiting}
    end.

ensure_enough_nodes(Mode, NNodes) ->
    Req = lists:foldl(fun ({_, Replicas}, Sum) -> Sum + Replicas end, 0, Mode),
    if NNodes < Req -> exit({not_enough_nodes_for_mode,
                             {req, Req}, {num_nodes, NNodes}});
       true         -> ok
    end.
