/* This script retrieves the information about running jobs */

function getJobs() {
	var jobs = new Array();
	
	var number = Math.round(Math.random() * 10);
	
	   for(i=0; i<number; i++) 
	   {
	   	 var job	= new Object();
	   	 job.name = "job" + i;
	   	 job.completed = Math.round(Math.random() * 100);
	   	 job.owner = "jobOwner" + i;
	   	 jobs[i] = job;	   	
   	}
	
	
	
	return jobs;
}
