
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.runtime.lib.http;

import java.util.Map;

import fisher.eval.EvalUtil;
import fisher.runtime.*;
import fisher.util.FisherException;

public  class  HTTPResponse extends HTTPMessage  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public final Thing responseCode; // 200 or 404 or something
	public final Thing meaning;      // OK or something
	public final RecordTh extraHeaders;
	
	public HTTPResponse(Thing responseCode, Thing meaning, Thing extraHeaders, Thing content) throws FisherException {
		this(HTTPRequest.HTTP_1_1, responseCode, meaning, EvalUtil.recordPlease(extraHeaders), EvalUtil.stringPlease(content));
	}
	public HTTPResponse(StringTh httpProtocolVersion, Thing responseCode, Thing meaning, RecordTh extraHeaders, StringTh content) throws FisherException {
		super(httpProtocolVersion, content, buildAllHeaders(content, extraHeaders));
		this.responseCode = responseCode;
		this.meaning = meaning;
		this.extraHeaders = extraHeaders;
	}
	
	public Thing responseCode() { return responseCode;} 
	public Thing meaning() {return meaning;}
	

	public String toString() {
		StringBuffer sb = new StringBuffer();
		// first line
		sb.append(
//				"&&Response!!\n" + 
				httpProtocolVersion + " " + responseCode + " " + meaning + "\n");
		// Remaining headings
		for(Map.Entry<String, Thing> end : allHeaders.fields.entrySet()) {
			final String headerName = end.getKey();
			if (headerWillBeComputed(headerName)) continue;
			sb.append(headerName + ": " + end.getValue() + "\n");
		}
		final String strContent = content.toString();
		sb.append("Content-Length: " + strContent.length() + "\n");
		// All-important blank line
		sb.append("\n");
		// Content

		sb.append(strContent);
		return sb.toString();		
	}
}
