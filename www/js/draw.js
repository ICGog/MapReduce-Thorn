function drawJob(job){
    var innerHTML = "<div class='foldable'><b>Job ID: " + job.id;
    innerHTML += " :</b><span id='progress'></span>% complete";
    innerHTML += "<div class='progressbar'></div>";
    innerHTML += "<div >Phase: <b><span id='phase'/></b></div></div>";
    innerHTML += "<div style='display:none;'>";
    innerHTML += "<div>Started: <span id='started_on'>" + job.started_on + "</span></div>";
    innerHTML += "<div style='display:none;'>Ended: <span id='ended_on'></span></div>";
    innerHTML += "<div class='button ui-button' id='mapBtn'>View map code</div>";
    innerHTML += "<div class='button ui-button' id='redBtn'>View reduce code</div>";
    //innerHTML += "<div class='button ui-button' id='killBtn'>Terminate job</div>";
    innerHTML += "</div>";
    
    if (job.has_ended) {
        var goal = "#old_joblist";
    }
    else {
        var goal = "#joblist";
    }
    
    $(goal).append($("<div/>").addClass("job").attr("id", job.id).append($("<div/>").html(innerHTML)));
    
	updateJobData(job);
	updateListeners();
    
    delete innerHTML;
}

function drawWorker(worker){
    var innerHTML = "<div class='worker_header' id='header'>" + worker.node + "</div>";
    innerHTML += "<div><div class='val' id='num_succ' title='Tasks succeeded'>" + worker.num_succ + "</div>";
    innerHTML += "<div class='val' id='num_failed' title='Tasks failed'>" + worker.num_failed + "</div>";
    innerHTML += "<div class='val' id='num_map_tasks' title='Map tasks'>" + worker.num_map_tasks + "</div>";
    innerHTML += "<div class='val' id='num_reduce_tasks' title='Reduce tasks'>" + worker.num_reduce_tasks + "</div>";
    innerHTML += "<div class='val wide' id='busy_time' title='Busy time'>" + worker.busy_time + "</div>";
    innerHTML += "<div class='foldable ui-icon ui-icon-triangle-1-e fold-button'></div>";
    innerHTML += "<div class='block' style='display:none;'><table border='0' width='95%'>";
    innerHTML += "<tr><td style='width: 50%'>Tasks succeeded:</td><td style='width: 50%'><span id='num_succ'>" + worker.num_succ + "</span></td></tr>";
    innerHTML += "<tr><td>Tasks failed:</td><td><span id='num_failed'>" + worker.num_failed + "</span></td></tr>";
    innerHTML += "<tr><td>Map tasks:</td><td><span id='num_map_tasks'>" + worker.num_map_tasks + "</span></td></tr>";
    innerHTML += "<tr><td>Reduce tasks:</td><td><span id='num_reduce_tasks'>" + worker.num_reduce_tasks + "</span></td></tr>";
    innerHTML += "<tr><td>Busy time:</td><td><span id='busy_time'>" + worker.busy_time + "</span></td></tr>";
    innerHTML += "<tr><td>Running job:</td><td><span id='exec_job_id'>" + worker.exec_job_id + "</span></td></tr>";
    innerHTML += "<tr><td>Last task:</td><td><span id='last_task_started_on'>" + worker.last_task_started_on + "</span></td></tr></table></div></div>";
    
    $("#worker_list").append($("<div/>").addClass("worker").attr("id", nameToID(worker.node)).append($("<div/>").html(innerHTML)));
}

function drawWorkerTooltips() {
	$('.val').tooltip({		
		delay: 0,
		track: true
	});
}

function displayKillDialog(job) {
	var dialog = confirm("Do you want to terminate job " + job.id);
	if(dialog) {
		var request = $.ajax({
			url: "/smr/smr_http:kill_job?job=" + job.id,
			async: true,
			success: function(data) {
				alert('Termination went well');
			},
			error: function(xhr){
				alert(xhr.status + ': ' + xhr.statusText);
			}
			});
		delete request;
	}
}