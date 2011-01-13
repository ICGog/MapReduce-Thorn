function convertDate(json) {	
	if(json == undefined || json == 'undefined')
		return "";
	
	var date = eval(json);
	
	if(date.hour < 10)
		date.hour = '0' + date.hour;
		
	if(date.minute < 10)
		date.minute = '0' + date.minute;
	
	if(date.second < 10)
		date.second = '0' + date.second;
	
	if(date.day < 10)
		date.day = '0' + date.day;
		
	if(date.month < 10)
		date.month = '0' + date.month;
	
	dateString = date.hour + ":" + date.minute + ":" + date.second;
	dateString += " " + date.day + "/" + date.month + "/" + date.year; 
	
	delete date;
	delete json;
	
	return dateString;
}

function prepareCode(code) {
	if (code == undefined) 
		return '';
	
	code = code.replace('<', '&lt;');
	code = code.replace('>', '&gt;');
	return code;
}

function nameToID(name){
    if( name != undefined)
	 	return name.replace('@', '-at-');
	else return '';
}

function idToName(id){
	if( id != undefined)
    	return id.replace('-at-', '@');
	else return '';
}

