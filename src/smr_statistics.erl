-module(smr_statistics).

-behaviour(gen_server).

-export([get_workers/0, get_jobs/0]).
-export([start_link/0, register_worker/1, unregister_worker/1, task_started/4,
         task_finished/2, task_failed/2, job_started/3, job_next_phase/3,
         job_finished/1, job_failed/2, pick_fastest_worker_of/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2,
        code_change/3]).

-include("smr.hrl").

-record(state, {workers = dict:new(),
                jobs = dict:new()}).

-define(NAME, {global, ?MODULE}).

%------------------------------------------------------------------------------
% Query API
%------------------------------------------------------------------------------

get_workers() ->
    gen_server:call(?NAME, get_workers).

get_jobs() ->
    gen_server:call(?NAME, get_jobs).

%------------------------------------------------------------------------------
% Internal API
%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?NAME, ?MODULE, [], []).

register_worker(Node) ->
    gen_server:cast(?NAME, {register_worker, Node}).

unregister_worker(Node) ->
    gen_server:cast(?NAME, {unregister_worker, Node}).

task_started(JobId, Node, TaskType, TaskSize) ->
    gen_server:cast(?NAME, {task_started, JobId, Node, TaskType, TaskSize}).

task_failed(JobId, Node) ->
    gen_server:cast(?NAME, {task_failed, JobId, Node}).

task_finished(JobId, Node) ->
    gen_server:cast(?NAME, {task_finished, JobId, Node}).

job_started(JobId, MapCode, ReduceCode) ->
    gen_server:cast(?NAME, {job_started, JobId, MapCode, ReduceCode}).

job_next_phase(JobId, Phase, InputSize) ->
    gen_server:cast(?NAME, {job_next_phase, JobId, Phase, InputSize}).

job_finished(JobId) ->
    gen_server:cast(?NAME, {job_finished, JobId}).

job_failed(JobId, Reason) ->
    gen_server:cast(?NAME, {job_failed, JobId, Reason}).

pick_fastest_worker_of(Workers) ->
    gen_server:call(?NAME, {pick_fastest_of, Workers}, infinity).

%------------------------------------------------------------------------------
% Handlers
%------------------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call(get_workers, _From, State = #state{workers = Workers}) ->
    {reply, dict:to_list(Workers), State};
handle_call(get_jobs, _From, State = #state{jobs = Jobs}) ->
    {reply, dict:to_list(Jobs), State};
handle_call({pick_fastest_of, Ws}, _From, State = #state{workers = Workers}) ->
    {_, Winner} = lists:max(lists:map(
        fun (W) ->
                case dict:find(W, Workers) of
                    {ok, #smr_worker{latest_performances = Perfs}} ->
                        {avg_performance(Perfs), W};
                    error ->
                        {-1.0, none}
                end
        end, Ws)),
    {reply, Winner, State}.

handle_cast({register_worker, W}, State) ->
    {noreply, internal_register_worker(W, State)};
handle_cast({unregister_worker, W}, State) ->
    {noreply, internal_unregister_worker(W, State)};
handle_cast({task_started, J, W, TaskType, TaskSize}, State) ->
    handle_update(
        W, J,
        fun (Worker = #smr_worker{num_map_tasks = NumMap,
                                  num_reduce_tasks = NumReduce},
             Job = #smr_job{using_workers = Ws}) ->
                Worker1 = Worker#smr_worker{exec_job_id = J,
                                            last_task_started_on = now(),
                                            last_task_size = TaskSize},
                Worker2 =
                    case TaskType of
                        map ->
                            Worker1#smr_worker{num_map_tasks = NumMap + 1};
                        reduce ->
                            Worker1#smr_worker{num_reduce_tasks = NumReduce + 1}
                    end,
                Job1 = Job#smr_job{using_workers =
                           ordsets:add_element(atom_to_bin(W), Ws)},
                {Worker2, Job1}
        end,
        State);
handle_cast({task_finished, J, W}, State) ->
    handle_update(
        W, J,
        fun (Worker = #smr_worker{num_succ = Succ,
                                  last_task_size = TSize,
                                  last_task_started_on = LTSO,
                                  latest_performances = Perfs},
             Job = #smr_job{phase = Phase,
                            map_input_size = MIS,
                            reduce_input_size = RIS,
                            progress = P,
                            phase_progress = PhaseP,
                            phase_worker_time_used_on_successful = PWTUSON}) ->
                WholeSize = case Phase of <<"map">>    -> MIS;
                                          <<"reduce">> -> RIS
                            end,
                DeltaPhaseP = TSize / WholeSize,
                NewP = P + 0.45 * DeltaPhaseP,
                NewPhaseP = PhaseP + DeltaPhaseP,
                DeltaT = timer:now_diff(now(), LTSO),
                NewPWTUSON = PWTUSON + DeltaT,
                WorkerEPIU = float(DeltaT) / DeltaPhaseP,
                JobEPIU = float(NewPWTUSON) / NewPhaseP,
                Perf = JobEPIU / WorkerEPIU,
                NewPerfs = add_performance(Perf, Perfs),
                task_ended_update(
                    Worker#smr_worker{num_succ = Succ + 1,
                                      latest_performances = NewPerfs},
                    Job#smr_job{progress = NewP,
                                phase_progress = NewPhaseP,
                                phase_worker_time_used_on_successful =
                                    NewPWTUSON})
        end,
        State);
handle_cast({task_failed, J, W}, State) ->
    handle_update(
        W, J,
        fun (Worker = #smr_worker{num_failed = Failed}, Job) ->
                task_ended_update(Worker#smr_worker{num_failed = Failed + 1},
                                  Job)
        end,
        State);
handle_cast({job_started, J, MapCode, ReduceCode}, State) ->
    {noreply, internal_register_job(J, MapCode, ReduceCode, State)};
handle_cast({job_next_phase, J, Phase, InputSize}, State) ->
    handle_update(
        none, J,
        fun (none, Job = #smr_job{}) ->
                Job1 = Job#smr_job{phase = atom_to_bin(Phase),
                                   phase_progress = 0.0,
                                   phase_worker_time_used_on_successful = 0},
                {none,
                 case Phase of
                     map    -> Job1#smr_job{map_input_size = InputSize,
                                            progress = 0.1};
                     reduce -> Job1#smr_job{reduce_input_size = InputSize,
                                           progress = 0.55}
                 end}
        end,
        State);
handle_cast({job_finished, J}, State) ->
    handle_update(
        none, J,
        fun (none, Job = #smr_job{}) ->
                {none, Job#smr_job{has_ended = true,
                                   progress = 1.0,
                                   ended_on = now(),
                                   phase = <<"output">>,
                                   outcome = <<"succeded">>}}
        end,
        State);
handle_cast({job_failed, J, Reason}, State) ->
    handle_update(
        none, J,
        fun (none, Job = #smr_job{}) ->
                Outcome = list_to_binary(io_lib:format("error: ~w", [Reason])),
                {none, Job#smr_job{has_ended = true,
                                   ended_on = now(),
                                   outcome = Outcome}}
        end,
        State).

handle_info({nodedown, W}, State) ->
    handle_update(
        W, none,
        fun (Worker, none) -> {Worker#smr_worker{is_dead = true}, none} end,
        State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%------------------------------------------------------------------------------
% Internal
%------------------------------------------------------------------------------

internal_register_worker(W, State = #state{workers = Workers}) ->
    monitor_node(W, true),
    State#state{workers =
        dict:store(W, #smr_worker{node = atom_to_bin(W)}, Workers)}.

internal_unregister_worker(W, State = #state{workers = Workers}) ->
    monitor_node(W, false),
    receive {nodedown, W} -> ok
    after 0 -> ok
    end,
    NewWorkers =
        dict:update(W,
                    fun (Worker) -> Worker#smr_worker{is_detached = true} end,
                    Workers),
    State#state{workers = NewWorkers}.

internal_register_job(J, MapCode, ReduceCode,
                      State = #state{jobs = Jobs}) ->
    MapCode1 = io_lib:format("~w~n", [MapCode]),
    ReduceCode1 = io_lib:format("~w~n", [ReduceCode]),
    State#state{jobs =
        dict:store(J, #smr_job{id = J,
                               started_on = now(),
                               map_code = list_to_binary(MapCode1),
                               reduce_code = list_to_binary(ReduceCode1)},
                   Jobs)}.

handle_update(W, J, FunUpdate,
              State = #state{workers = Workers, jobs = Jobs}) ->
    Worker = case W of none -> none;
                       _    -> dict:fetch(W, Workers)
             end,
    Job = case J of none -> none;
                    _    -> dict:fetch(J, Jobs)
          end,
    {NewWorker, NewJob} = FunUpdate(Worker, Job),
    State1 = case NewWorker of
                 none -> State;
                 _    -> State#state{workers = dict:store(W, NewWorker,
                                                          Workers)}
             end,
    State2 = case NewJob of
                 none -> State1;
                 _    -> State1#state{jobs = dict:store(J, NewJob, Jobs)}
             end,
    {noreply, State2}.

task_ended_update(Worker = #smr_worker{node = WBin,
                                       busy_time = BTime,
                                       last_task_started_on = LTSO},
                  Job = #smr_job{using_workers = Ws,
                                 total_worker_time_used = TWTU}) ->
    DeltaT = timer:now_diff(now(), LTSO),
    {Worker#smr_worker{exec_job_id = 0,
                       busy_time = BTime + DeltaT},
     Job#smr_job{using_workers = ordsets:del_element(WBin, Ws),
                 total_worker_time_used = TWTU + DeltaT}}.

atom_to_bin(A) ->
    list_to_binary(atom_to_list(A)).

add_performance(P, [P1, P2, P3, P4, _P5]) ->
    [P, P1, P2, P3, P4];
add_performance(P, Perfs) ->
    [P | Perfs].

avg_performance(Perfs) ->
    avg_performance(Perfs, 0.0, 0).

avg_performance([], _, 0) ->
    1000000.0; %% A large number so it will get picked
avg_performance([], Sum, Length) ->
    Sum / float(Length);
avg_performance([P | Perfs], Sum, Length) ->
    avg_performance(Perfs, Sum + P, Length + 1).
