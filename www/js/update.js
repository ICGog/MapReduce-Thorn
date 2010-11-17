/* This script updates the presented data */

function initialize()
{	
	$("#framework_name").append(" 0.1");		
	$("#logo").click( function()	  {
		  	$("img").fadeTo("slow",0);	
	  });  	
  
	update();	
}

function update()
{
		var ns = getNodes();
		var js = getJobs();
	
	   $(".nodelist").empty();
	   
	   if(!ns.length)
   	$(".nodelist")
   	.append($("<div/>").text("No nodes available"));
   
	  
	for(var n in ns) {
		        
        $(".nodelist")
        .append($("<div/>").addClass("node")
        .append($("<div/>").addClass("name").text(ns[n].name))
        .append($("<div/>").addClass("value succ").text(ns[n].succ))
        .append($("<div/>").addClass("value fail").text(ns[n].fail))
        .append($("<div/>").addClass("value busy").text(ns[n].busy))
        .append($("<div/>").addClass("value map").text(ns[n].map))
		  .append($("<div/>").addClass("value red").text(ns[n].red)));	   
	} 		
	
	
	setTimeout("update()", 5000);
}