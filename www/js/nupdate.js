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

function update() {
	
	
}