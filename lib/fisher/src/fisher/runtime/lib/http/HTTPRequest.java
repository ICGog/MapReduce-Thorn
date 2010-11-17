
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

import java.awt.PageAttributes.OriginType;
import java.io.Reader;
import java.io.Serializable;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.Set;
import java.util.Map.Entry;

import javax.management.modelmbean.RequiredModelMBean;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.runtime.*;
import fisher.runtime.lib.socketeer.ReplySocketTh;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;


public  class  HTTPRequest extends HTTPMessage implements Serializable  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
/*
Typical HTTP request looks like: 
GET /yog.html?a=Aye&b=Bye  HTTP/1.1
Accept: text/html, text/squid

\r\n\r\n
*/
	public StringTh verb;       // GET
	public StringTh resource;	// "/yog.html"
	public RecordTh params;     // {: a:"Aye", b:"Bye" :}
	
	// public RecordTh headers;    // a record of lists of strings
	
	
	public final ReplySocketTh replyGoesTo; 
	
	@Override
	public boolean transmissible() {
		return true;
	}
	
	public static final StringTh HTTP_1_1 = StringTh.of("HTTP/1.1");
	// This ctor is for calling from Thorn
	public HTTPRequest(Thing verb, Thing resource, Thing params, Thing headers, Thing content) throws FisherException {
		super(HTTP_1_1, EvalUtil.stringPlease(content), EvalUtil.recordPlease(headers));
		this.verb = (StringTh) verb;
		this.resource = (StringTh) resource;
		this.params = (RecordTh) params;
		this.replyGoesTo = null;
	}
	
	// This ctor is used internally
	public HTTPRequest(StringTh verb, StringTh resource, RecordTh params, StringTh httpProtocolVersion, 
			RecordTh headers,
			StringTh content, ReplySocketTh replyGoesTo) {
		super(httpProtocolVersion, content, headers);
		this.verb = verb;
		this.resource = resource;
		this.params = params;
		this.replyGoesTo = replyGoesTo;
	}

	public static StringTh makeContent(ListTh originalTextReversed) {
		ListTh rev = originalTextReversed.reversed();
		StringTh joined = StringTh.of(Bard.sep(rev, "\n"));
		return joined;
	}

	
	
	public String toString() {
		String paramString;
		if (params == null) return "";
		final Set<Entry<String, Thing>> paramSet = params.fieldSet();
		if (paramSet.isEmpty()) paramString = "";
		else {
			StringBuffer sb = new StringBuffer();
			sb.append("?");
			boolean first = true;
			for (Entry<String, Thing> entry : paramSet) {
				if (first) first=false;
				else {
					sb.append("&");
				}
				sb.append(entry.getKey());
				sb.append("=");
				String v = EvalUtil.toString(entry.getValue());
				try {
					sb.append(URLEncoder.encode(v, "UTF-8"));
				} catch (UnsupportedEncodingException e) {
					Doom.runtimeNonThorn("Sheesh, is UTF-8 broken?", Evaller.lastSyntax(), params);
					e.printStackTrace();
				}
			}
			paramString = sb.toString();
		}
		String requestLine = verb + " " + resource + paramString + " " + httpProtocolVersion;
		String headerLines = Bard.sep2(headers().fieldSet(), ":", "\n");
		String newline = "\n";
		String content = EvalUtil.toString(this.content);
		int contentLength = content.length();
		String headerContentLength = "Content-Length: " + contentLength + "\n";
		String res = 
			requestLine + newline + headerLines + newline + headerContentLength + newline + content + newline;
		return res;
		
	}
	
	public Thing verb() { return verb;}
	public Thing protocol() {return httpProtocolVersion;} 
	public Thing resource() {return resource;}
	public Thing params() {return params;} 
//	public Thing original() {
//		if (cachedOriginalText == null) {
//			cachedOriginalText = originalTextReversed.reversed();
//			originalTextReversed = null; // Don't need it any more
//		}
//		return StringTh.of("&&Request!!\n"+cachedOriginalText);
//	}
	
	public Thing buildResponse(Thing responseCodePlusMeaning, Thing content, Thing extraHeaders) throws FisherException {
		StringTh scontent = content.str();
		checkArg(responseCodePlusMeaning, RecordTh.class, null);
		RecordTh rcm = (RecordTh) responseCodePlusMeaning; 
		checkArg(extraHeaders, RecordTh.class, null);
		RecordTh extrah = (RecordTh) extraHeaders;
		HTTPMessage response = new HTTPResponse(httpProtocolVersion, rcm.getField("code", null), rcm.getField("meaning", null), extrah,
				scontent);
		return response;
	}
	
	public Thing respond(Thing responseCodePlusMeaning, Thing content, Thing extraHeaders) throws FisherException {
		Thing response = this.buildResponse(responseCodePlusMeaning, content, extraHeaders);
		return replyGoesTo.send(response, null);
	}
	
	public boolean equals(Object other) {
		if (!super.equals(other)) return false;
		HTTPRequest rother = (HTTPRequest) other;
		try {
			return EvalUtil.eq(this.params(), rother.params()) && 
				EvalUtil.eq(this.resource(), rother.resource()) &&
				EvalUtil.eq(this.verb(), rother.verb());
		} catch (FisherException e) {
			return false;
		}
	}
	
	
	
}
