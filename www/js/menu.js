var selected;
var previous;
var doUpdate = true;
var updateTimeout;

var counter = 0;

var content = {};

function init_menu(){
    $.ajaxSetup({
        async: false,
        cache: false,
    });
    
    $('.display').click(function(){
        $('.selected').removeClass('selected');
        $(this).addClass('selected');
        previous = selected;
        selected = $(this).get(0).id;
        reloadContents();
    });
    
    $('#update_btn').click(function(){
        doUpdate = !doUpdate;
        if (doUpdate) {
            $("#update_btn").text("[Disable Updates]");
            update();
        }
        else {
            $("#update_btn").text("[Enable Updates]");
            clearTimeout(updateTimeout);
        }
    });
    
    // Load content
    $('#pages').children().each(function(index, value){
        $.get('blocks/' + value.id + '.html', function(data){
            content[value.id] = data;
            //w = window.open('blocks/' + value.id + '.html', 'mapCode', 'width=500,height=400,location=yes,resizeable=no');
        });
    });
    
    // Select the default page
    $('.display:first').addClass('selected');
    selected = $('.display:first').get(0).id;
    
    $('.main').html(content[selected]);
    
    update();
}

function reloadContents(){
    content[previous] = $('.main').html();
    $('.main').html(content[selected]);
	update();
}

function update(){
	var js = getJobs();
	var ws = getWorkers();
   
	updateJobs(js);
	
	$('#worker_count').text(ws.size());
	$('#completed_count').text(js.size() - js.running);
	$('#jobs_running').text(js.running);	
    
    //START Foreign code. source: jQueryUI
    $('.progressbar').each(function(){
        var value = parseInt($(this).text());
        $(this).empty().progressbar({
            value: value
        });
    });
    //END Foreign code
    
    if (doUpdate) 
        updateTimeout = setTimeout('update()', 5000);
}


function updateJobs(js){
    
    
    $('.job').each(function(index, value){
        var id = parseInt(value.id); // no other way to do it, grr
        var j = js.get(id);
        
        if (j == null) {
            $(this).parent().remove();
        }
        else {
            updateJob(j);
        }
        
        js.remove(id);
    });
    
    js.each(function(key, value){
        drawJob(value);
    });
}

function updateJob(job){
    var loc = $('#' + job.id);
    $('#phase', loc).html(job.phase);
    $('.progressbar', loc).replaceWith("<div class='progressbar'>" + job.progress + "</div>");
    $('#progress', loc).html(job.progress);   
	
	 
    if (!loc.hasClass('ended') && job.has_ended) {
		var tree = loc.get(0);
		loc.remove();
		$("#old_joblist").prepend(tree);
		
		
        if (job.outcome == 'succeded') {
			loc.addClass('ended succeeded');    
        }
        else {
        	loc.addClass('ended failed');
        }
    }
		
}

function drawJob(job){

    var innerHTML = "<div class='job' id='" + job.id + "'>";
    innerHTML += "<div class='foldable'><b>Job ID: " + job.id;
    innerHTML += " :</b><span id='progress'></span>% complete";
    innerHTML += "<div class='progressbar'></div></div>";
    innerHTML += "<div>";
    innerHTML += "<div>Phase: <b><span id='phase'/></b></div>";
    innerHTML += "<div class='button' id='MapCode'" + job.id + "'>[View map code]</div>";
    innerHTML += "<div class='button' id='RedCode'" + job.id + "'>[View reduce code]</div>";
    innerHTML += "<div class='button' id='KillBtn" + job.id + "'>[Kill job]</div>";
    innerHTML += "</div></div>";
	
	if(job.has_ended) 
		$("#old_joblist").append($('<div/>').html(innerHTML));
	else    
    	$("#joblist").append($('<div/>').html(innerHTML));
	
	//var pos = $('#' + id);
	
	/*$('.foldable', pos).click(function(){
        $(this).next().toggle('fast');
        return false;
    }).next().hide();*/
	
	updateJob(job);	
    
    delete innerHTML;
}



