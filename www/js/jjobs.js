// testing needs to be done when server side is done
// test data in /js/testjobs.js
// url would be changed once know specific location

function getJobs() {

$.get("testjobs.js",
  function(data){
    var number = data.length;
    var current;
    var result = new Array();
    for(current = 0; current<number; current++) {
      var temp = data[current];
      var job = new Object();
      job.id = temp.id;
      job.is_completed = temp.is_completed;
      job.progress = temp.progress;
      job.started_on = temp.started_on;
      job.map_code = temp.map_code;
      job.reduce_code = temp.reduce_code;
      result[current] = job;
    }
    return result;
});

