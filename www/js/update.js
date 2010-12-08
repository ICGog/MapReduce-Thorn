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
	
	$( "#dialog-confirm" ).dialog({
			autoOpen: false,
			resizable: false,
			height:200,
			width: 400,
			modal: true,
			buttons: {
				"Kill, please": function() {
					$( this ).dialog( "close" );
				},
				"No, thanks": function() {
					$( this ).dialog( "close" );
				}
			}
	});
	
	$("#show_log").click(function() 
			{
				codeWindow=window.open('log/smr.log','Log','width=500,height=400,location=no,resizeable=no');
		        codeWindow.focus();
			});
		
	update();	
}

function update()
{
		
	updateJobs();
	updateWorkers();	
	
	$('.foldable').click(function() 
	{
		$(this).next().toggle('fast');
		return false;
	});
	
	$('.node_header').click(function() 
			{
				$(this).next().toggle('fast');
				return false;
			});
	
	//$("#workers_header").next().hide();
	
	
	if(doUpdate)
	{
	  updateTimeout = setTimeout("update()", 2000);
	}	
}

function updateJobs() {
	js = getJobs();
		
	$(".joblist").empty();
	$(".old_joblist").empty();	
		
	if(!js.length)
		$(".joblist")
		.append($("<div/>").text("No running jobs"));
		
	for(var j in js) 
	{
		job = js[j];
		
		jobname = job.id;
		owner = job.owner;
		id = jobname;
		
		
		innerHTML = "<div class=\"foldable\"><b>Job ID: " + jobname + "</b> : " + job.progress + "% complete";
		innerHTML += "<div class=\"progressbar\">" + job.progress + "</div></div>";
		
		if (job.has_ended) {
			innerHTML += "<div>";
			innerHTML += "<div>Owner: <b>" + owner + "</b></div>";
			innerHTML += "</div>";
			
			$(".old_joblist").append(
					$("<div/>").addClass("job old").attr("id", "acc").append(
							$("<div/>").html(innerHTML)));
			
		} else {

			innerHTML += "<div>";
			innerHTML += "<div>Phase: <b>" + job.phase + "</b></div>";
			//innerHTML += "<div>Used workers: <b>" + eval('(' + job.using_workers +')') + "</b></div>";
			innerHTML += "<div class=\"button\" id=\"MapCode" + id+ "\">[View map code]</div>";
			innerHTML += "<div class=\"button\" id=\"RedCode" + id+ "\">[View reduce code]</div>";
			innerHTML += "<div class=\"button\" id=\"" + id+ "\">[Kill job]</div>";
			innerHTML += "</div>";

			$(".joblist").append(
					$("<div/>").addClass("job").attr("id", "acc").append(
							$("<div/>").html(innerHTML)));

			$('#MapCode' + id)
					.click(
							function() {
								codeWindow = window
										.open('', 'mapCode',
												'width=500,height=400,location=no,resizeable=no');
								codeWindow.document.write("<code>");
								codeWindow.document.write(job.map_code);
								codeWindow.document.write("</code>");
								codeWindow.focus();
								return false;
							});

			$('#RedCode' + id)
					.click(
							function() {
								codeWindow = window
										.open('', '',
												'width=500,height=400,location=no,resizeable=no');
								codeWindow.document.write("<code>");
								codeWindow.document.write(job.reduce_code);
								codeWindow.document.write("</code>");
								codeWindow.focus();
								return false;
							});

			$('#' + id).click(function() {
				killJob(this.id);
			});
		}
        
	} 
	
	//START Foreign code
	$('.progressbar').each(function() 
	{
             var value = parseInt($(this).text());
            $(this).empty().progressbar({value: value});
    }); 
    //END Foreign code

}

function updateWorkers() {
    ns = getNodes();
		
 	$(".nodelist").empty();
	   
   	if(!ns.length)
   		$(".nodelist")
   		.append($("<div/>").text("No workers available"));
   
	  
	for(var n in ns) 
	{ 
		/*$(".nodelist")
        .append($("<div/>").addClass("node").attr("id", "node" + n)
            .append($("<div/>").addClass("name").text(ns[n].node))
            .append($("<div/>").addClass("value").text(ns[n].num_succ))
            .append($("<div/>").addClass("value").text(ns[n].num_failed))
            .append($("<div/>").addClass("value").text(ns[n].num_map_jobs))
            .append($("<div/>").addClass("value").text(ns[n].num_reduce_jobs))
            .append($("<div/>").addClass("value").text(ns[n].busy_time))
            .append($("<div/>").addClass("value").text(ns[n].exec_job_id))
            .append($("<div/>").addClass("value").text(ns[n].last_task_started_on))
            .append($("<div/>").addClass("value").text("[kill]"))                  
        );*/
		
		var innerHTML = "<div class=\"node_header\">" + ns[n].node + "</div>";
		innerHTML +=	"<div><div class=\"val\">" + ns[n].num_succ + "</div>";
		innerHTML +=	"<div class=\"val\">" + ns[n].num_failed + "</div>";
		innerHTML +=	"<div class=\"val\">" + ns[n].num_map_tasks + "</div>";
		innerHTML +=	"<div class=\"val\">" + ns[n].num_reduce_tasks + "</div>"; 
		innerHTML +=	"<div class=\"val\">" + ns[n].busy_time + "</div>";
		innerHTML +=	"<div class=\"val foldable\">" + ns[n].exec_job_id + "</div>";
		//innerHTML +=	"<div class=\"val \">" + ns[n].last_task_started_on + "</div></div>";
		
		//innerHTML += "<div class=\"node_header\">" + ns[n].node + "</div>";
		innerHTML +=	"<div><table border=\"0\">";			
		innerHTML +=	"<tr width=300em><td>Tasks succeeded:</td><td>" + ns[n].num_succ + "</td></tr>";
		innerHTML +=	"<tr><td>Tasks failed:</td><td>" + ns[n].num_failed + "</td></tr>";
		innerHTML +=	"<tr><td>Map tasks:</td><td>" + ns[n].num_map_tasks + "</td></tr>";
		innerHTML +=	"<tr><td>Reduce tasks:</td><td>" + ns[n].num_reduce_tasks + "</td></tr>"; 
		innerHTML +=	"<tr><td>Busy time:</td><td>" + ns[n].busy_time + "</td></tr>";
		innerHTML +=	"<tr><td>Running job:</td><td>" + ns[n].exec_job_id + "</td></tr>";
		innerHTML +=	"<tr><td>Last task:</td><td>" + ns[n].last_task_started_on + "</td></tr></table></div></div>";
		
		
		
		$(".nodelist").append(
				$("<div/>").addClass("node").attr("id", "node" + n).append(
						$("<div/>").html(innerHTML)));
	} 		

}	

function killJob(jobname) {
	$( "#dialog-confirm" ).text("Are you sure you want to kill " + jobname + "?");
	$("#dialog-confirm").dialog('open');	
}

