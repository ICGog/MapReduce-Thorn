// testing needs to be done when server side is done
// test data in /js/testworkers.js
// url would be changed once know specific location

function getNodes() {

$.get("testworkers.js",
  function(data){
    var number = data.length;
    var current;
    var result = new Array();
    for(current = 0; current<number; current++) {
      var temp = data[current];
      var worker = new Object();
      worker.name = temp.name;
      worker.starton = temp.started_on;
      worker.exec = temp.num_exec;
      worker.succ = temp.num_succ;
      worker.fail = temp.num_failed;
      worker.busy = temp.busy_time;
      worker.map = temp.num_map_jobs;
      worker.red = temp.num_reduce_jobs;
      worker.starttime = temp.start_time;
      result[current] = worker;
    }
    return result;
});

