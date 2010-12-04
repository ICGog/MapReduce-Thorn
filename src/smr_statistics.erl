-module(smr_statistics).

-behaviour(gen_server).

-export([get_workers/0, get_jobs/0]).
-export([start_link/0, register_worker/1, task_started/4, task_finished/2,
         task_failed/2, job_started/3, job_next_phase/3, job_finished/1,
         job_failed/2]).
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

%------------------------------------------------------------------------------
% Handlers
%------------------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call(get_workers, _From, State = #state{workers = Workers}) ->
    {reply, dict:to_list(Workers), State};
handle_call(get_jobs, _From, State = #state{jobs = Jobs}) ->
    {reply, dict:to_list(Jobs), State}.

handle_cast({register_worker, Node}, State) ->
    {noreply, internal_register_worker(Node, State)};
handle_cast({task_started, J, W, TaskType, TaskSize}, State) ->
    handle_update(
        W, J,
        fun (Worker = #smr_worker{num_map_tasks = NumMap,
                                  num_reduce_tasks = NumReduce}) ->
                Worker1 = Worker#smr_worker{exec_job_id = J,
                                            last_task_started_on = now(),
                                            last_task_size = TaskSize},
                case TaskType of
                    map    -> Worker1#smr_worker{num_map_tasks = NumMap + 1};
                    reduce -> Worker1#smr_worker{num_reduce_tasks =
                                                     NumReduce + 1}
                end
        end,
        fun (Job = #smr_job{using_workers = Ws}) ->
                Job#smr_job{using_workers = ordsets:add_element(atom_to_bin(W),
                                                                Ws)}
        end,
        State);
handle_cast({task_finished, J, W}, State = #state{workers = Workers}) ->
    handle_update(
        W, J,
        fun (Worker = #smr_worker{num_succ = Succ}) ->
                task_ended_update_w(Worker#smr_worker{num_succ = Succ + 1})
        end,
        fun (Job = #smr_job{phase = Phase,
                            map_input_size = MIS,
                            reduce_input_size = RIS,
                            progress = P}) ->
            #smr_worker{last_task_size = TSize} = dict:fetch(W, Workers),
            WholeSize = case Phase of <<"map">>    -> MIS;
                                      <<"reduce">> -> RIS
                        end,
            NewP = P + 0.45 * TSize / WholeSize,
            task_ended_update_j(Job#smr_job{progress = NewP}, W)
        end,
        State);
handle_cast({task_failed, J, W}, State) ->
    handle_update(
        W, J,
        fun (Worker = #smr_worker{num_failed = Failed}) ->
                task_ended_update_w(Worker#smr_worker{num_failed = Failed + 1})
        end,
        fun (Job) -> task_ended_update_j(Job, W) end,
        State);
handle_cast({job_started, J, MapCode, ReduceCode}, State) ->
    {noreply, internal_register_job(J, MapCode, ReduceCode, State)};
handle_cast({job_next_phase, J, Phase, InputSize}, State) ->
    handle_update(
        none, J, none,
        fun (Job = #smr_job{}) ->
                Job1 = Job#smr_job{phase = atom_to_bin(Phase)},
                case Phase of
                    map    -> Job1#smr_job{map_input_size = InputSize,
                                           progress = 0.1};
                    reduce -> Job1#smr_job{reduce_input_size = InputSize,
                                           progress = 0.55}
                end
        end,
        State);
handle_cast({job_finished, J}, State) ->
    handle_update(
        none, J, none,
        fun (Job = #smr_job{}) ->
                Job#smr_job{has_ended = true,
                            progress = 1.0,
                            ended_on = now(),
                            phase = <<"output">>,
                            outcome = <<"succeded">>}
        end,
        State);
handle_cast({job_failed, J, Reason}, State) ->
    handle_update(
        none, J, none,
        fun (Job = #smr_job{}) ->
                Outcome = list_to_binary(io_lib:format("error: ~w", [Reason])),
                Job#smr_job{has_ended = true,
                            ended_on = now(),
                            outcome = Outcome}
        end,
        State).

handle_info({nodedown, W}, State) ->
    handle_update(
        W, none,
        fun (Worker) -> Worker#smr_worker{is_dead = true} end,
        none, State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%------------------------------------------------------------------------------
% Internal
%------------------------------------------------------------------------------

internal_register_worker(Node, State = #state{workers = Workers}) ->
    monitor_node(Node, true),
    State#state{workers =
        dict:store(Node, #smr_worker{node = atom_to_bin(Node)}, Workers)}.

internal_register_job(JobId, MapCode, ReduceCode,
                      State = #state{jobs = Jobs}) ->
    MapCode1 = io_lib:format("~w~n", [MapCode]),
    ReduceCode1 = io_lib:format("~w~n", [ReduceCode]),
    State#state{jobs =
        dict:store(JobId, #smr_job{id = JobId,
                                   started_on = now(),
                                   map_code = list_to_binary(MapCode1),
                                   reduce_code = list_to_binary(ReduceCode1)},
                   Jobs)}.

handle_update(W, J, FunUpdateWorker, FunUpdateJob,
              State = #state{workers = Workers, jobs = Jobs}) ->
    State1 = case W of none -> State;
                       _    -> State#state{workers =
                                   dict:update(W, FunUpdateWorker, Workers)}
             end,
    State2 = case J of none -> State1;
                       _    -> State1#state{jobs =
                                   dict:update(J, FunUpdateJob, Jobs)}
             end,
    {noreply, State2}.

task_ended_update_w(Worker = #smr_worker{busy_time = BTime,
                                         last_task_started_on = LTSO}) ->
    Worker#smr_worker{exec_job_id = 0,
                      busy_time = BTime +
                                      (timer:now_diff(now(), LTSO) div 1000)}.

task_ended_update_j(Job = #smr_job{using_workers = Ws}, W) ->
    Job#smr_job{using_workers = ordsets:del_element(atom_to_bin(W), Ws)}.

atom_to_bin(A) ->
    list_to_binary(atom_to_list(A)).
