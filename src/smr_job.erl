
-module(smr_job).

-behavior(gen_server).

-export([start_link/5, add_input/2, start/1, result/3, task_started/3]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {sup,
                id,
                phase = input, % input | map | reduce
                map_fun,
                reduce_fun,
                ongoing = 0,
                input = [],
                result,
                map_batch_size,
                reduce_batch_size}).

%------------------------------------------------------------------------------
% Internal API
%------------------------------------------------------------------------------

start_link(MapFun, ReduceFun, JobId, MapBatchSize, ReduceBatchSize) ->
    gen_server:start_link(?MODULE, [self(), MapFun, ReduceFun, JobId,
                                    MapBatchSize, ReduceBatchSize], []).

add_input(Job, Input) ->
    gen_server:cast(Job, {add_input, Input}).

start(Job) ->
    gen_server:cast(Job, start).

result(Job, Worker, Result) ->
    gen_server:cast(Job, {result, Worker, Result}).

task_started(Job, Worker, Size) ->
    gen_server:call(Job, {task_started, Worker, Size}, infinity).

%------------------------------------------------------------------------------
% Handlers
%------------------------------------------------------------------------------

init([Sup, MapFun, ReduceFun, JobId, MapBatchSize, ReduceBatchSize]) ->
    error_logger:info_msg("Job ~p created~n", [JobId]),
    smr_statistics:job_started(JobId, MapFun, ReduceFun),
    {ok, #state{sup = Sup,
                id = JobId,
                map_fun = MapFun,
                reduce_fun = ReduceFun,
                map_batch_size = MapBatchSize,
                reduce_batch_size = ReduceBatchSize}}.

handle_call({task_started, WorkerPid, Size}, _From,
            State = #state{phase = Phase, id = JobId}) ->
    erlang:monitor(process, WorkerPid),
    smr_statistics:task_started(JobId, node(WorkerPid), Phase, Size),
    {reply, ok, State}.

handle_cast(start, State) ->
    {noreply, set_phase(map, State)};
handle_cast({add_input, NewInput}, State = #state{phase = input,
                                                  input = CurInput}) ->
    {noreply, State#state{input = NewInput ++ CurInput}};
handle_cast({result, Worker, Result}, State0 = #state{id = JobId}) ->
    smr_statistics:task_finished(JobId, node(Worker)),
    State1 = #state{ongoing = Ongoing} = agregate_result(Result, State0),
    State2 = State1#state{ongoing = Ongoing - 1},
    case State2 of #state{ongoing = 0} -> handle_phase_finished(State2);
                   _                   -> {noreply, State2}
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

handle_phase_finished(State = #state{phase = map, result = Result}) ->
    {noreply, set_phase(reduce, State#state{input = dict:to_list(Result)})};
handle_phase_finished(State = #state{phase = reduce,
                                     result = Result,
                                     id = JobId}) ->
    smr_statistics:job_finished(JobId),
    smr_master:job_result(self(), Result),
    {stop, normal, State}.

set_phase(map, State = #state{input = Input, id = JobId}) ->
    error_logger:info_msg("Job ~p: map phase started~n", [JobId]),
    smr_statistics:job_next_phase(JobId, map, length(Input)),
    send_tasks(Input, State#state{phase = map,
                                  input = undefined, % free
                                  result = dict:new()});
set_phase(reduce, State = #state{input = Input, id = JobId}) ->
    error_logger:info_msg("Job ~p: reduce phase started~n", [JobId]),
    smr_statistics:job_next_phase(JobId, reduce, length(Input)),
    send_tasks(Input, State#state{phase = reduce,
                                  input = undefined, % free
                                  result = []}).

agregate_result(Result, State = #state{phase = map, result = Dict}) ->
    State#state{result =
        lists:foldl(fun ({K, V}, DictAcc) ->
                            dict:update(K, fun (Old) -> [V | Old] end, [V],
                                        DictAcc)
                    end, Dict, Result)};
agregate_result(Result, State = #state{phase = reduce, result = List}) ->
    State#state{result = Result ++ List}.

send_tasks([], State) ->
    State;
send_tasks(Input, State = #state{sup = Sup,
                                 phase = Phase,
                                 map_fun = MapFun,
                                 reduce_fun = ReduceFun,
                                 ongoing = Ongoing,
                                 map_batch_size = MBS,
                                 reduce_batch_size = RBS}) ->
    {TaskInput, RestInput} = split_input(case Phase of map    -> MBS;
                                                       reduce -> RBS
                                         end, Input),
    {TaskType, Fun} = case Phase of map    -> {map, MapFun};
                                    reduce -> {reduce, ReduceFun}
                      end,
    JobPid = self(),
    spawn_link(
        fun () ->
                case smr_task_sup_sup:start_task(
                         smr_job_sup:task_sup_sup(Sup),
                         JobPid, TaskType, Fun, TaskInput) of
                     {ok, _}         -> ok;
                     {error, Reason} -> exit(Reason)
                end
        end),
    send_tasks(RestInput, State#state{ongoing = Ongoing + 1}).

split_input(N, Input) ->
    split_input(N, Input, []).

split_input(0, Input, Before) ->
    {Before, Input};
split_input(_, [], Before) ->
    {Before, []};
split_input(N, [KV | RestInput], Before) ->
    split_input(N - 1, RestInput, [KV | Before]).
