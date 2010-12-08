// testing needs to be done when server side is done
// test data in /js/testjobs.js
// url would be changed once know specific location

function getJobs() {
var result = new Array();
$.ajax({
  url: "smr/smr_http:get_jobs",
  /*url: "js/testjobs.js",*/
  dataType:'json',
  async:false,
  success:
  function(data){
    var number = data.length;
    var current;
    for(current = 0; current<number; current++) {
      var temp = data[current];
      var job = new Object();      
      job.id = temp.id;
      job.has_ended = temp.has_ended;
      job.phase_progress = Math.round(temp.phase_progress * 100);
      job.progress = Math.round(temp.progress * 100);
      job.started_on = convertDate(temp.started_on);
      job.ended_on = convertDate(temp.ended_on);
      job.phase_worker_time_used_on_successful = temp.phase_worker_time_used_on_successful;
      job.total_worker_time_used = temp.total_worker_time_used;
      job.phase = temp.phase;
      job.map_code = temp.map_code;    
      job.reduce_code = temp.reduce_code;
      job.map_input_size = temp.map_input_size;
      job.reduce_input_size = temp.reduce_input_size;
      job.using_workers = temp.using_workers;
      job.outcome = temp.outcome;
      result[current] = job;
    }
}});
return result;
}
