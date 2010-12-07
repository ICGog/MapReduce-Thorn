// testing needs to be done when server side is done
// test data in /js/testworkers.js
// url would be changed once know specific location

function getNodes() {
var aResult = new Array();
$.ajax({
  url: "smr/smr_http:get_workers",
  dataType:'json',
  async:false,
  success:
  function (data){
    var number = data.length;
    var current;
    for(current = 0; current<number; current++) {
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
      
      aResult[current] = worker;
    }
}});
return aResult;
}

function convertDate(json) {
	
	var date = eval(json);
	
	dateString = date.hour + ":" + date.minute + ":" + date.second;
	dateString += " " + date.day + "/" + date.month + "/" + date.year; 
	
	return dateString;
}
