var selected;
var previous;
var doUpdate = true;
var doExpand = true;
var updateTimeout;
var updateInterval = 5000;

var counter = 0;

var content = {};

function init_menu(){
    $.ajaxSetup({
        async: false,
        cache: false,
    });
    
    $("#interval").text(updateInterval / 1000 + " s");
    
    $("#slider").slider({
        min: 1000,
        max: 10000,
        step: 1000,
        value: 5000,
        change: function(event, ui){
            $("#interval").text(ui.value / 1000 + " s");
            updateInterval = ui.value;
        }
    });
    
    init_buttons();
    
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
    
    $('.foldable').click(function(){
        $(this).next().toggle('fast');
        return false;
    }).next().hide();
    
    
    $("#dialog-confirm").dialog({
        autoOpen: false,
        resizable: false,
        height: 200,
        width: 400,
        modal: true,
        buttons: {
            "Yes": function(){
                $(this).dialog("close");
            },
            "No": function(){
                $(this).dialog("close");
            }
        }
    });
    
    
    
    update();
}

function init_buttons(){
    $('.action').button();
    
    $('.display').click(function(){
		if($(this).hasClass('selected'))
			return;
					
        $('.selected').removeClass('selected');		
        $(this).addClass('selected');
        previous = selected;
        selected = $(this).get(0).id;
        reloadContents();
    });
    
    $('#update_btn').click(function(){
        doUpdate = !doUpdate;
        if (doUpdate) {
			clearTimeout(updateTimeout);
            $("#update_btn").button("option", "label", "Disable Updates");
            update();
        }
        else {
            $("#update_btn").button("option", "label", "Enable Updates");
            clearTimeout(updateTimeout);
        }
    });
	
    $("#expand_btn").click(function(){
        if (doExpand) {
            $("#expand_btn").button("option", "label", "Collapse all");
            $('.foldable').next().show('fast');
            $('.ui-icon-triangle-1-e').removeClass('ui-icon-triangle-1-e').addClass('ui-icon-triangle-1-s');
            
        }
        else {
            $("#expand_btn").button("option", "label", "Expand all");
            $('.foldable').next().hide('fast');
            $('.ui-icon-triangle-1-s').removeClass('ui-icon-triangle-1-s').addClass('ui-icon-triangle-1-e');
        }
        doExpand = !doExpand;
    });
    
    $("#log_btn").click(function(){
        var codeWindow = window.open('log/smr.log', 'Log', 'width=600,height=400,location=0');
		codeWindow.document.title = "SMR Log";
        codeWindow.focus();
    });
}

function reloadContents(){
    content[previous] = $('.main').html();
    $('.main').html(content[selected]);
    update();
	updateListeners();
}

function update(){
    var js = getJobs();
    var ws = getWorkers();
    
    if($('.jobs').length > 0)
		updateJobs(js);
	if($('#worker_list').length > 0)
    	updateWorkers(ws);
    
    $('#worker_count').text(ws.size());
    $('#completed_count').text(js.size() - js.running);
    $('#jobs_running').text(js.running);
    $('#av_busy_time').text(ws.avBusyTime);
	$('#busiest_worker').text(ws.busiest);
	$('#max_busy').text(ws.maxBusyTime);
	$('#idlest_worker').text(ws.idlest);
	$('#min_busy').text(ws.minBusyTime);
	$('#failing_worker').text(ws.mostFailing);
    
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

function updateListeners() {
	 $('.foldable').unbind().click(function(){
        $(this).next().toggle('fast');
        return false;
    });
    
    $('.fold-button').unbind().click(function(){
        if ($(this).hasClass('ui-icon-triangle-1-s')) 
            $(this).removeClass('ui-icon-triangle-1-s').addClass('ui-icon-triangle-1-e');
        else 
            $(this).removeClass('ui-icon-triangle-1-e').addClass('ui-icon-triangle-1-s');
        
        
        
        $(this).next().toggle('fast');        
    });
}


function updateJobs(js){
    $('.job').each(function(index, value){
        var id = parseInt(value.id);
        var j = js.get(id);
        
        if (j == null) {
            $(this).remove();
        }
        else {
            updateJobData(j);
        }
        
        js.remove(id);
    });
    
    js.each(function(key, value){
        drawJob(value);
    });
}

function updateJobData(job){
    var loc = $('#' + job.id);
    $('#phase', loc).html(job.phase);
    $('.progressbar', loc).replaceWith("<div class='progressbar'>" + job.progress + "</div>");
    $('#progress', loc).html(job.progress);    
    
    if (!loc.hasClass('ended') && job.has_ended) {
        $('#ended_on', loc).text(job.ended_on).parent().show();
        
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
	
	updateJobButtons(job);
}

function updateJobButtons(job) {
	var loc = $('#' + job.id);
	$("#mapBtn", loc).button().unbind().click(function(){
		var mapCodeWindow = window.open('about:blank', '', 'width=600,height=400,location=0');
		mapCodeWindow.document.write(job.map_code);
		mapCodeWindow.document.title = 'Map code for job ' + job.id;
        mapCodeWindow.focus();
    
    });
    $("#redBtn", loc).button().unbind().click(function(){
		var reduceCodeWindow = window.open('about:blank', '', 'width=600,height=400,location=0');
        reduceCodeWindow.document.write(job.reduce_code);
		reduceCodeWindow.document.title = 'Reduce code for job ' + job.id;
		reduceCodeWindow.focus();    
    });
    $("#killBtn", loc).button().unbind().click(function(){
        displayKillDialog(job);
    });
}


function updateWorkers(ws){

    $('.worker').each(function(index, value){
        var w = ws.get(idToName(value.id));
        
        if (w == null) {
            $(this).remove();
        }
        else {
            updateWorkerData(w);
        }
        
        ws.remove(idToName(value.id));
    });
    
    
    ws.each(function(key, value){
        drawWorker(value);
    });

}

function updateWorkerData(worker){
    var loc = $('#' + nameToID(worker.node));
    
    if (worker.exec_job_id) 
        $("#header", loc).html(worker.node + ' running job ' + worker.exec_job_id).addClass('working');
    else 
        $("#header", loc).html(worker.node).removeClass('working');
    
    $('#num_succ', loc).html(worker.num_succ);
    $('#num_failed', loc).html(worker.num_failed);
    $('#num_map_tasks', loc).html(worker.num_map_tasks);
    $('#num_reduce_tasks', loc).html(worker.num_reduce_tasks);
    $('#busy_time', loc).html(worker.busy_time);
    $('#exec_job_id', loc).html(worker.exec_job_id);
    $('#last_task_started_on', loc).html(worker.last_task_started_on);
}

