// testing needs to be done when server side is done
// test data in /js/teststats.js
// url would be changed once know specific location

function getJobs() {

$.get("teststats.js",
  function(data){
    var number = data.length;
    var current;
    var result = new Array();
    for(current = 0; current<number; current++) {
      var temp = data[current];
      var stat = new Object();
      stat.started_on = temp.started_on;
      stat.total_completed_jobs = temp.total_completed_jobs;
      stat.total_workers = temp.total_workers;
      stat.total_running_jobs = temp.total_running_jobs;
      stat.average_busy_time = temp.average_busy_time;
      result[current] = stat;
    }
    return result;
});

