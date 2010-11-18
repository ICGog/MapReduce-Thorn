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
	
	update();	
}

function update()
{
		ns = getNodes();
		js = getJobs();
		
	$(".joblist").empty();
		
	if(!js.length)
		$(".joblist")
		.append($("<div/>").text("No running jobs"));
		
	for(var j in js) 
	{
		jobname = js[j].name;
		owner = js[j].owner;
		id = j + jobname;
		
		
		innerHTML = "<div class=\"foldable\"><b>" + jobname + "</b> : " + js[j].completed + "% complete";
		innerHTML += "<div class=\"progressbar\">" + js[j].completed + "</div></div>";
		innerHTML += "<div>";
		innerHTML += "<div>Owner: <b>" + owner + "</b></div>";	
		innerHTML += "<div id=\"M" + id + "\">[View map code]</div>";
		innerHTML += "<div id=\"R" + id + "\">[View reduce code]</div>";
		innerHTML += "<div>[Kill job]</div>";	
		innerHTML += "</div>";	
	
	 	$(".joblist")
        .append(
           $("<div/>").addClass("job").attr("id", "acc")
        .append($("<div/>").html(innerHTML)));      
        
      	$('#M' + id).click(function() 
      	{      	 
		 $(this).append($("<div/>").load("\ #map_code")).show();			 	 
		 return false;		
		});
		
		$('#R' + id).click(function() 
		{		 
		 $(this).append($("<div/>").load("\ #reduce_code"));			 	 
		 return false;		
		});
        
	} 
	
	//START Foreign code
	$('.progressbar').each(function() 
	{
             var value = parseInt($(this).text());
            $(this).empty().progressbar({value: value});
    }); 
    //END Foreign code
		
 	$(".nodelist").empty();
	   
   	if(!ns.length)
   		$(".nodelist")
   		.append($("<div/>").text("No workers available"));
   
	  
	for(var n in ns) 
	{
		        
        $(".nodelist")
        .append($("<div/>").addClass("node")
        .append($("<div/>").addClass("name").text(ns[n].name))
        .append($("<div/>").addClass("value succ").text(ns[n].succ))
        .append($("<div/>").addClass("value fail").text(ns[n].fail))
        .append($("<div/>").addClass("value busy").text(ns[n].busy))
        .append($("<div/>").addClass("value map").text(ns[n].map))
		  .append($("<div/>").addClass("value red").text(ns[n].red)));	   
	} 		
	
	
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
