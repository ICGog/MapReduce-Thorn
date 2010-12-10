
%------------------------------------------------------------------------------
% General
%------------------------------------------------------------------------------

-define(WWW_PORT, 55156).

-define(CHUNK_SIZE, 8388608). %% 64 MiB / 32 MiB in words on a
                              %% 64bit / 32bit machine

-define(WORKER_SUB_PROCESSES, 2).

-define(REPLICATION_MODE, [{n_ram_copies, 1}]).

%------------------------------------------------------------------------------
% Statistics and HTTP API
%------------------------------------------------------------------------------

-record(smr_worker, {node,
                     is_dead = false,
                     is_detached = false,
                     num_failed = 0,
                     num_succ = 0,
                     num_map_tasks = 0,
                     num_reduce_tasks = 0,
                     busy_time = 0, %% In microseconds
                     exec_job_id = 0, %% 0 means no job executing atm
                     last_task_started_on, %% now()
                     latest_performances = [], %% last 5 results of (job effort per input unit) / (worker effort per input unit)
                     last_task_size = 0
                    }).

-record(smr_job, {id,
                  has_ended = false,
                  phase_progress = 0.0,
                  progress = 0.0, %% 0.0 - 0.1 input, 0.1 - 0.55 map, 0.55 - 1.0 reduce
                  started_on, %% now()
                  ended_on, %% now()
                  phase_worker_time_used_on_successful = 0,
                  total_worker_time_used = 0,
                  phase = <<"input">>, %% <<"input">> | <<"map">> |
                                       %%   <<"reduce">> | <<"output">>
                  map_code = <<"undefined">>,
                  reduce_code = <<"undefined">>,
                  map_input_size = 0,
                  reduce_input_size = 0,
                  using_workers = ordsets:new(),
                  outcome = <<"not finished">> %% <<"not finished">> |
                                               %%   <<"succeded">> |
                                               %%   <<"error: ...">>
                 }).
