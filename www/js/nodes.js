/* This script retrieves the information about nodes */

/* This is a stub of a function retrieving the node data		*/

function getNodes() {
		var nodes = new Array();	
		var number = Math.round(Math.random() * 10);
	
	   for(i=0; i<number; i++) 
	   {
	   	 var worker	= new Object();
	   	 worker.node = "worker" + i;
	   	 worker.num_exec = i;	   	 
	   	 worker.num_failed = i;
	   	 worker.num_succ = i;
	   	 worker.num_map_tasks = i;
	   	 worker.num_reduce_tasks = i;
	   	 worker.busy_time = "00:00";

	   	 
	   	 nodes[i] = worker;	   	
   	}
	
		return nodes;
}

