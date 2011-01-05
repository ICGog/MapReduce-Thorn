// testing needs to be done when server side is done
// test data in /js/testworkers.js
// url would be changed once know specific location

function getWorkers(){
    var ws = new Hashtable();
    
    var sum = 0;
    
    ws.minBusyTime = Number.MAX_VALUE;
    ws.maxBusyTime = 0;
    ws.maxFails = 0;
    ws.idlest = 'none';
    ws.busiest = 'none';
    ws.mostFailing = 'none';
    
    $.ajax({
		url: "smr/smr_http:get_workers",
		dataType: 'json',
		async: false,
		success: function(data){
			if (data == null) 
				return;
			
			
			for (var current = 0; current < data.length; current++) {
				var temp = data[current];
				var worker = new Object();
				
				worker.node = temp.node;
				worker.is_dead = temp.is_dead;
				worker.num_failed = temp.num_failed;
				worker.num_succ = temp.num_succ;
				worker.num_map_tasks = temp.num_map_tasks;
				worker.num_reduce_tasks = temp.num_reduce_tasks;
				worker.busy_time = temp.busy_time;
				worker.exec_job_id = temp.exec_job_id;
				worker.last_task_started_on = convertDate(temp.last_task_started_on);
				
				sum += worker.busy_time;
				
				if (worker.busy_time > ws.maxBusyTime) {
					ws.maxBusyTime = worker.busy_time;
					ws.busiest = worker.node;
				}
				if (worker.busy_time < ws.minBusyTime) {
					ws.minBusyTime = worker.busy_time;
					ws.idlest = worker.node;
				}
				if (worker.num_failed > ws.maxFails) {
					ws.maxFails = worker.num_failed;
					ws.mostFailing = worker.node;
				}
				
				ws.put(temp.node, worker);
			}
			
			if (data.length > 0) 
				ws.avBusyTime = (sum / data.length);
			else 
				ws.avBusyTime = 0;
			
		}});
    
    if (ws.maxBusyTime <= ws.minBusyTime) {
        ws.idlest = 'none';
        ws.minBusyTime = '0';
    }
    
    
    return ws;
}
