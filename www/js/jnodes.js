// testing needs to be done when server side is done
/* assume data return from server : 
  {"nodes":[
     {"name":name,"succ":succ,"fail":fail,"busy":busy,"map":map,"red":red},
     {"name":name,"succ":succ,"fail":fail,"busy":busy,"map":map,"red":red}]}
*/

function getResultNodes() {

$.get("http://localhost:8081/smr/smr_http:get_all_workers",
        function(data){
	  var nodes = data.nodes;
	  var number = nodes.length;
          var current;
          var result = new Array();
          for(current = 0; current<number; current++) {
            var cnode = nodes[current];
            var worker = new Object();
	    worker.name = "worker" + cnode.name;
            worker.succ = cnode.succ;
	    worker.fail = cnode.fail;
            worker.busy = cnode.busy;
            worker.map = cnode.map;
            worker.red = cnode.red;
            result[current] = worker;
          }
          return result;
});

