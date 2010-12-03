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
	}).next().hide();
	
	if(doUpdate)
	{
	  updateTimeout = setTimeout("update()", 20000);
	}	
}

function updateJobs() {
	js = getJobs();
		
	$(".joblist").empty();
		
	if(!js.length)
		$(".joblist")
		.append($("<div/>").text("No running jobs"));
		
	for(var j in js) 
	{
		jobname = js[j].name;
		owner = js[j].owner;
		id = jobname;
		
		
		innerHTML = "<div class=\"foldable\"><b>" + jobname + "</b> : " + js[j].completed + "% complete";
		innerHTML += "<div class=\"progressbar\">" + js[j].completed + "</div></div>";
		innerHTML += "<div>";
		innerHTML += "<div>Owner: <b>" + owner + "</b></div>";	
		innerHTML += "<div class=\"button\" id=\"M" + id + "\">[View map code]</div>";
		innerHTML += "<div class=\"button\" id=\"R" + id + "\">[View reduce code]</div>";
		innerHTML += "<div class=\"button\" id=\"" + id + "\">[Kill job]</div>";	
		innerHTML += "</div>";	
	
	 	$(".joblist")
        .append(
           $("<div/>").addClass("job").attr("id", "acc")
        .append($("<div/>").html(innerHTML)));      
        
      	$('#M' + id).click(function() 
      	{      	 
		 //$(this).append($("<div/>").load("\ #map_code")).show();	
		 codeWindow=window.open('','','width=500,height=400,location=no,resizeable=no')
         codeWindow.document.write("This is the map code")         
         codeWindow.focus()		
 		 return false;		
		});
		
		$('#R' + id).click(function() 
		{		 
		 //$(this).append($("<div/>").load("\ #reduce_code"));
		 codeWindow=window.open('','','width=500,height=400,location=no,resizeable=no')
         codeWindow.document.write("This is the reduce code")         
         codeWindow.focus()		 	 
		 return false;		
		});
		
		$('#' + id).click(		
		function() 
		{									
			killJob(this.id);
		});
        
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
        $(".nodelist")
        .append($("<div/>").addClass("node")
        .append($("<div/>").addClass("name").text(ns[n].node))
        .append($("<div/>").addClass("value").text(ns[n].num_exec))
        .append($("<div/>").addClass("value").text(ns[n].num_failed))
        .append($("<div/>").addClass("value").text(ns[n].num_succ))
        .append($("<div/>").addClass("value").text(ns[n].num_map_tasks))
        .append($("<div/>").addClass("value").text(ns[n].num_reduce_tasks))
		.append($("<div/>").addClass("value").text(ns[n].busy_time)));	   
	} 		

}

function killJob(jobname) {
	$( "#dialog-confirm" ).text("Are you sure you want to kill " + jobname + "?");
	$("#dialog-confirm").dialog('open');	
}
