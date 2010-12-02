
-record(worker, {node, 
                 num_exec = 0,
                 num_failed = 0,
                 num_succ = 0,
                 num_map_tasks = 0,
                 num_reduce_tasks = 0,
                 busy_time = 0}).

