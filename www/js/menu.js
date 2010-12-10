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
    
    update();
}

function init_buttons(){
    $('.action').button();
    
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
            $("#update_btn").button("option", "label", "Disable Updates");
            update();
        }
        else {
            $("#update_btn").button("option", "label", "Enable Updates");
            clearTimeout(updateTimeout);
        }
    });
    
    $('#plus').click(function(){
        if (updateInterval <= 0) 
            $('#minus').addClass('button');
        
        updateInterval += 1000;
        $("#interval").text(updateInterval);
    });
    
    $('#minus').click(function(){
        if (updateInterval > 0) {
            updateInterval -= 1000;
            $("#interval").text(updateInterval);
            if (updateInterval <= 0) {
                $('#minus').removeClass('button');
            }
        }
    });
    
    $("#expand_btn").click(function(){
        if (doExpand) {
            $("#expand_btn").button("option", "label", "Collapse all");
            $('.foldable').next().show('fast');
        }
        else {
            $("#expand_btn").button("option", "label", "Expand all");
            $('.foldable').next().hide('fast');
        }
        doExpand = !doExpand;
    });
    
    $("#log_btn").click(function(){
        codeWindow = window.open('log/smr.log', 'Log', 'width=600,height=400,location=no,resizeable=no');
        codeWindow.focus();
    });
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
    updateWorkers(ws);
    
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
    
    
    $('.foldable').unbind().click(function(){
        $(this).next().toggle('fast');
        return false;
    });
    
    if (doUpdate) 
        updateTimeout = setTimeout('update()', 5000);
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
    
}

function drawJob(job){
    var innerHTML = "<div class='foldable'><b>Job ID: " + job.id;
    innerHTML += " :</b><span id='progress'></span>% complete";
    innerHTML += "<div class='progressbar'></div>";
    innerHTML += "<div >Phase: <b><span id='phase'/></b></div></div>";
    innerHTML += "<div style='display:none;'>";
    innerHTML += "<div>Started: <span id='started_on'>" + job.started_on + "</span></div>";
    innerHTML += "<div style='display:none;'>Ended: <span id='ended_on'></span></div>";
    innerHTML += "<div class='button' id='MapCode'" + job.id + "'>[View map code]</div>";
    innerHTML += "<div class='button' id='RedCode'" + job.id + "'>[View reduce code]</div>";
    innerHTML += "<div class='button' id='KillBtn" + job.id + "'>[Kill job]</div>";
    innerHTML += "</div>";
    
    if (job.has_ended) {
        var goal = "#old_joblist";
    }
    else {
        var goal = "#joblist";
    }
    
    $(goal).append($("<div/>").addClass("job").attr("id", job.id).append($("<div/>").html(innerHTML)));
    
    updateJobData(job);
    
    delete innerHTML;
}

function updateWorkers(ws){

    $('.worker').each(function(index, value){
        var w = ws.get(value.id);
        
        if (w == null) {
            $(this).remove();
        }
        else {
            updateWorkerData(w);
        }
        
        ws.remove(value.id);
    });
    
    
    ws.each(function(key, value){
        drawWorker(value);
    });
}

function drawWorker(worker){
    var innerHTML = "<div class='worker_header'>" + worker.node + "</div>";
    innerHTML += "<div><div class='val' id='num_succ'>" + worker.num_succ + "</div>";
    innerHTML += "<div class='val' id='num_failed'>" + worker.num_failed + "</div>";
    innerHTML += "<div class='val' id='num_map_tasks'>" + worker.num_map_tasks + "</div>";
    innerHTML += "<div class='val' id='num_reduce_tasks'>" + worker.num_reduce_tasks + "</div>";
    innerHTML += "<div class='val' id='busy_time'>" + worker.busy_time + "</div>";
    innerHTML += "<div class='val foldable' id='exec_job_id'>" + worker.exec_job_id + "</div>";
    innerHTML += "<div style='display:none;'><table border='0'>";
    innerHTML += "<tr width=300em><td>Tasks succeeded:</td><td><span id='num_succ'>" + worker.num_succ + "</span></td></tr>";
    innerHTML += "<tr><td>Tasks failed:</td><td><span id='num_failed'>" + worker.num_failed + "</span></td></tr>";
    innerHTML += "<tr><td>Map tasks:</td><td><span id='num_map_tasks'>" + worker.num_map_tasks + "</span></td></tr>";
    innerHTML += "<tr><td>Reduce tasks:</td><td><span id='num_reduce_tasks'>" + worker.num_reduce_tasks + "</span></td></tr>";
    innerHTML += "<tr><td>Busy time:</td><td><span id='busy_time'>" + worker.busy_time + "</span></td></tr>";
    innerHTML += "<tr><td>Running job:</td><td><span id='exec_job_id'>" + worker.exec_job_id + "</span></td></tr>";
    innerHTML += "<tr><td>Last task:</td><td><span id='last_task_started_on'>" + worker.last_task_started_on + "</span></td></tr></table></div></div>";
    
    $("#worker_list").append($("<div/>").addClass("worker").attr("id", worker.node).append($("<div/>").html(innerHTML)));
}

function updateWorkerData(worker){
    var queryStr = worker.node;
    queryStr = "#" + queryStr.replace('@', '\\\\@') + " ";
    
    //var loc = $('#' + queryStr).toggle();
    $(queryStr + '#num_succ').html(worker.num_succ);
    $(queryStr + '#num_failed').html(worker.num_failed);
    $(queryStr + '#num_map_tasks').html(worker.num_map_tasks);
    $(queryStr + '#num_reduce_tasks').html(worker.num_reduce_tasks);
    $(queryStr + '#busy_time').html(worker.busy_time);
    $(queryStr + '#exec_job_id').html(worker.exec_job_id);
    $(queryStr + '#last_task_started_on').html(worker.last_task_started_on);
}
