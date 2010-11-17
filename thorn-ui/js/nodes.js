/* This script retrieves the information about nodes */

/* This is a stub of a function retrieving the node data		*/
function getNodes() {
		var nodes = new Array();	
		var number = Math.round(Math.random() * 10);
	
	   for(i=0; i<number; i++) 
	   {
	   	 var worker	= new Object();
	   	 worker.name = "worker" + i;
	   	 worker.succ = i;
	   	 worker.fail = i;
	   	 worker.busy = "00:00";
	   	 worker.map = i;
	   	 worker.red = i;
	   	 
	   	 nodes[i] = worker;	   	
   	}
	
		return nodes;
}
