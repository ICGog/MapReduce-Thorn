
-module(smr_job).

-behavior(gen_server).

-export([start_link/4, result/3]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {phase, % map | reduce
                map_fun,
                reduce_fun,
                master,
                spare_workers = [],
                ongoing = dict:new(),
                input,
                result}).

-define(MAP_BATCH_SIZE, 1).
-define(REDUCE_BATCH_SIZE, 1).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

start_link(Master, MapFun, ReduceFun, Input) ->
    {ok, Job} =
        gen_server:start_link(?MODULE, [Master, MapFun, ReduceFun, Input], []),
    error_logger:info_msg("Job ~p started~n", [Job]),
    gen_server:cast(Job, start),
    {ok, Job}.

result(Job, Worker, Result) ->
    gen_server:cast(Job, {result, Worker, Result}).

%------------------------------------------------------------------------------
% Handlers
%------------------------------------------------------------------------------

init([Master, MapFun, ReduceFun, Input]) ->
    {ok, #state{input = Input,
                map_fun = MapFun,
                reduce_fun = ReduceFun,
                master = Master}}.

handle_call(_Request, _From, State) ->
    {stop, unexpected_call, State}.

handle_cast(start, State) ->
    {noreply, set_phase(map, State)};
handle_cast({result, Worker, Result}, State0) ->
    State1 = agregate_result(Result, State0),
    case remove_ongoing(Worker, State1) of
        {all_finished, State2} -> handle_phase_finished(State2);
        {ongoing, State3}      -> {noreply, State3}
    end.

handle_info(Msg, State) ->
    {stop, {unexpected_message, Msg}, State}.

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
                              master = Master}) ->
    smr_master:job_result(Master, self(), Result),
    {stop, normal, State}.

set_phase(map, State = #state{input = Input}) ->
    error_logger:info_msg("Job ~p: map phase started~n", [self()]),
    send_jobs(Input, State#state{phase = map,
                                 input = undefined, % free
                                 result = dict:new()});
set_phase(reduce, State = #state{input = Input}) ->
    error_logger:info_msg("Job ~p: reduce phase started~n", [self()]),
    send_jobs(Input, State#state{phase = reduce,
                                 input = undefined, % free
                                 result = []}).

agregate_result(Result, State = #state{phase = map, result = Dict}) ->
    State#state{result =
        lists:foldl(fun ({K, V}, DictAcc) ->
                            case dict:is_key(K, DictAcc) of
                                true  -> dict:append(K, V, DictAcc);
                                false -> dict:store(K, [V], DictAcc)
                            end
                    end, Dict, Result)};
agregate_result(Result, State = #state{phase = reduce, result = List}) ->
    State#state{result = Result ++ List}.

remove_ongoing(Worker, State = #state{ongoing = Ongoing0}) ->
    Ongoing1 = dict:update_counter(Worker, -1, Ongoing0),
    Ongoing2 = case dict:fetch(Worker, Ongoing1) of
                   0 -> dict:erase(Worker, Ongoing1);
                   _ -> Ongoing1
               end,
    {case dict:size(Ongoing2) of 0 -> all_finished;
                                 _ -> ongoing
     end, State#state{ongoing = Ongoing2}}.

send_jobs([], State) ->
    State;
send_jobs(Input, State0 = #state{phase = Phase,
                                 map_fun = MapFun,
                                 reduce_fun = ReduceFun,
                                 ongoing = Ongoing}) ->
    {Worker, State1} = spare_worker(State0),
    {JobInput, RestInput} = lists:split(?MAP_BATCH_SIZE, Input),
    {JobType, Fun} = case Phase of map    -> {map, MapFun};
                                   reduce -> {reduce, ReduceFun}
                     end,
    smr_worker:do_job(Worker, self(), JobType, Fun, JobInput),
    send_jobs(RestInput, State1#state{ongoing =
                             dict:update_counter(Worker, 1, Ongoing)}).

spare_worker(State = #state{spare_workers = [], master = Master}) ->
    spare_worker(State#state{spare_workers =
                     smr_master:allocate_workers(Master)});
spare_worker(State = #state{spare_workers = [Worker | RestWorkers]}) ->
    {Worker, State#state{spare_workers = RestWorkers}}.

