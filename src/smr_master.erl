-module(smr_master).
-behavior(gen_server).

-export([map_reduce/4, job_result/3, allocate/1]).
-export([start_link/0, spawn_worker/2, shutdown_worker/2, print_workers/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, 
        code_change/3]).

-record(state, {workers = dict:new(), jobs = dict:new()}).

-record(worker, {node,
                 pid,
                 num_executing = 0,
                 num_failed = 0,
                 num_succ = 0}).

-record(job, {pid, from, map_fun, reduce_fun, input}).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

start_link() -> gen_server:start_link(?MODULE, [], []).

spawn_worker(Pid, Node) -> 
    gen_server:call(Pid, {spawn_worker, Node}, infinity).

shutdown_worker(Pid, Node) -> 
    gen_server:call(Pid, {shutdown_worker, Node}, infinity).

print_workers(Pid) -> gen_server:call(Pid, print_workers, infinity).

job_result(Pid, JobPid, Result) -> 
    gen_server:cast(Pid, {job_result, JobPid, Result}).

allocate(Pid) ->
    gen_server:call(Pid, allocate, infinity).

map_reduce(Pid, Map, Reduce, Input) -> 
    gen_server:call(Pid, {map_reduce, Map, Reduce, Input}, infinity).

%------------------------------------------------------------------------------
% Handlers
%------------------------------------------------------------------------------

init([]) -> process_flag(trap_exit, true), {ok, #state{}}.

handle_call({spawn_worker, WorkerNode}, _From,
            State = #state{workers = Workers}) ->
    case dict:find(WorkerNode, Workers) of
        {ok, _Val} ->
             {reply, {error, already_registered}, State};
        error ->
             WorkerPid = proc_lib:spawn_link(
                 WorkerNode,
                 fun () ->
                         {ok, WorkerState} = smr_worker:init([]),
                         gen_server:enter_loop(smr_worker, [], WorkerState)
                 end),
             NewWorkers = dict:store(WorkerNode, #worker{node = WorkerNode,
                                                         pid = WorkerPid},
                                     Workers),
             {reply, ok, State#state{workers = NewWorkers}}
    end;

handle_call({shutdown_worker, WorkerNode}, _From,
            State = #state{workers = Workers}) ->
    case dict:find(WorkerNode, Workers) of
        {ok, _Val} -> {reply, ok,
                       State#state{workers = dict:erase(WorkerNode, Workers)}};
        error      -> {reply, {error, not_registered}, State}
    end;

handle_call(print_workers, _From, State) ->
    {reply, dict:fetch_keys(State#state.workers), State};

handle_call(allocate, _From, State = #state{workers = Workers}) ->
    {reply, dict:fold(fun (_, #worker{pid = Pid}, Acc) -> [Pid | Acc] end,
                      [], Workers),
     State};

handle_call({map_reduce, Map, Reduce, Input}, From, 
            State = #state{jobs = Jobs}) ->
    {ok, JobPid} = smr_job:start_link(self(), Map, Reduce, Input),
    {noreply, State#state{jobs = dict:store(JobPid, #job{pid = JobPid,
                                                         from = From,
                                                         map_fun = Map,
                                                         reduce_fun = Reduce,
                                                         input = Input},
                                            Jobs)}}.

handle_cast({job_result, JobPid, Result}, State = #state{jobs = Jobs}) ->
    gen_server:reply((dict:fetch(JobPid, Jobs))#job.from, Result),
    {noreply, State}.

handle_info({'EXIT', JobPid, normal},
            State = #state{jobs = Jobs}) ->
    {noreply, State#state{jobs = dict:erase(JobPid, Jobs)}};
    
handle_info(_Message, State) -> {stop, unexpected_message, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.
