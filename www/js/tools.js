function convertDate(json) {	
	if(json == "undefined")
		return "";
	
	var date = eval(json);
	
	dateString = date.hour + ":" + date.minute + ":" + date.second;
	dateString += " " + date.day + "/" + date.month + "/" + date.year; 
	
	delete date;
	delete json;
	
	return dateString;
}

// (c) linkibol.com - http://blog.linkibol.com/2010/05/07/
//	did-you-know-that-jquery-leaks-memory-like-a-fountain/

var xhr = new XMLHttpRequest();
var url = 'index.php';

function openXHR(){
	xhr.open('POST',url, false);
	xhr.setRequestHeader('X-Requested-With','XMLHttpRequest');
	xhr.setRequestHeader('Accept', 'text/javascript, text/html, application/xml, text/xml, */*');
	xhr.onreadystatechange = readyStateChanged;
	xhr.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
	xhr.send('');
}

function nill(){}
function readyStateChanged(){
	var counter = 0;
	
	if(xhr.readyState === 4){
			if(counter>10000){ return; }
			var theDiv = document.getElementById('TestDiv');
			if(theDiv){ theDiv.innerHTML = (counter++);	}
			xhr.onreadystatechange = nill;
			xhr.abort();
			setTimeout(openXHR,1);
	}
}