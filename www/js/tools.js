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

