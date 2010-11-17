/* This script updates the presented data */

function initialize()
{	
	$("#framework_name").append(" 0.1");			  
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
		
	for(var j in js) {
		jobname = js[j].name;
		owner = js[j].owner;
		id = "j_" + jobname;
		
		
		innerHTML = "<div class=\"foldable\"><b>" + jobname + "</b> : " + js[j].completed + "% complete";
		innerHTML += "<div class=\"progressbar\">" + js[j].completed + "</div></div>";
		innerHTML += "<div>";
		innerHTML += "<div>Owner: <b>" + owner + "</b></div>";	
		innerHTML += "<div><a href=\"code.txt\">[View map code]</a> [View reduce code] [Kill job]</div>";	
		innerHTML += "</div>";

	
	 	$(".joblist")
        .append(
           $("<div/>").addClass("job").attr("id", "acc")
        .append($("<div/>").html(innerHTML)));          
	} 
	
	
	$('.progressbar').each(function() {
             var value = parseInt($(this).text());
            $(this).empty().progressbar({value: value});
        }); 
	
	
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
	
	
	$('.foldable').click(function() {
		$(this).next().toggle('fast');
		return false;
	}).next().hide();
	
	
	setTimeout("update()", 20000);
}
