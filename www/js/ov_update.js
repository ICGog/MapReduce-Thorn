/* This script updates the presented data */

var doUpdate = true;
var updateTimeout;

function initialize()
{	
	$("#framework_name").append(" 0.1");		
	
	$("#expand_jobs").click(function() 
	{
		$('.foldable').next().toggle('fast');				
	});
	
	$(".expander").click(function() 
	{
		$(this).next().toggle('fast');				
	});
	
	
	$("#update_toggle").click(function() 
	{
		doUpdate = !doUpdate;
		if(doUpdate)		
		{			
			$("#update_toggle").text("[Disable Updates]");
			update();
		} 
		else 
		{			
			$("#update_toggle").text("[Enable Updates]");
			clearTimeout(updateTimeout);		
		}
	});	
	
	$("#show_log").click(function() 
	{
		codeWindow=window.open('log/smr.log','Log','width=500,height=400,location=no,resizeable=no');
        codeWindow.focus();
	});
	
	$("#logo").tooltip();	
	
	update();	
}

function update()
{
	updateJobs();
	updateWorkers();	
	
	stat = getStats();
	
	$("#start_time").text(stat.start_time);
	//$("#completed_count").text(stat.completed_count);
	$("#busy_time").text(stat.busy_time);
	
	$('.foldable').click(function() 
	{	    
		$(this).next().toggle('fast');
		return false;				
	}).next().hide(); 
	
	if(doUpdate)
	{
	  updateTimeout = setTimeout("update()", 20000);
	}	
}

function updateJobs() {
	js = getJobs();
	
	running = 0;
	complete = 0;
	
	if(js.length > 0)
		for(var j in js) {
			if(js[j].has_ended)
				complete++;
			else
				running++;
		}	
	
	$("#jobs_running").text(running);
	$("#completed_count").text(complete);
}

function updateWorkers() {
    ns = getNodes();	
    $("#worker_count").text(ns.length);	
   
}
