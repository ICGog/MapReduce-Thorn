package magne;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Serializable;
import java.net.URI;
import java.util.ArrayList;

import org.apache.commons.fileupload.MultipartStream;

import com.sun.net.httpserver.Headers;
import com.sun.net.httpserver.HttpExchange;

import fisher.runtime.BytesTh;
import fisher.runtime.ListTh;
import fisher.runtime.RecordTh;
import fisher.runtime.StringTh;
import fisher.runtime.Thing;
import fisher.runtime.ThingExtended;
import fisher.util.FisherException;

public class SimpleHTTPRequest extends ThingExtended implements Serializable{

	public StringTh method;
	public StringTh path;
	public StringTh query;
	public RecordTh headers;
	public StringTh ip;
	public BytesTh content; // What about bytes in multipart post?

	public HttpExchange ex;

	public SimpleHTTPRequest(HttpExchange e) throws IOException{
		super();

		method = StringTh.of(e.getRequestMethod());

		// URI path and query
		URI uri = e.getRequestURI();
		path = StringTh.of(uri.getPath());
		query = uri.getQuery() == null ? StringTh.of("") : StringTh.of(uri.getQuery());
		ip = StringTh.of(e.getRemoteAddress().getAddress().toString());

		// Request Headers
		Headers hs = e.getRequestHeaders();
		headers = RecordTh.unfinished();

		try{
			for(String h : hs.keySet()){
				// not sure if they already are lower case...
				headers.setField(h.toLowerCase(), StringTh.of(hs.getFirst(h)));
			}
		}
		catch(FisherException ex){
			// not going to happen
			ex.printStackTrace();
		}
		headers.finish();

		ByteArrayOutputStream out = new ByteArrayOutputStream();
		BufferedInputStream in = new BufferedInputStream(e.getRequestBody());

		byte[] data = new byte[4096]; 
		int lgd = in.read(data);
		while (lgd != -1){
			out.write(data, 0, lgd);
			lgd = in.read(data);
		}

		content = new BytesTh(out.toByteArray());

		// for responding later
		ex = e;
	}

	@Override
	public boolean transmissible() {
		return true;
	}

	public Thing content(){
		return content;
	}

	public Thing headers(){
		return headers;
	}

	public Thing method(){
		return method;
	}

	public Thing path(){
		return path;
	}

	public Thing query() {
		return query;
	}

	public Thing ip(){
		return ip;
	}

	public void respond(Thing status, Thing content, Thing headers) throws FisherException, IOException {
		checkArg(headers, ListTh.class, null);

		byte[] scontent;
		if(content instanceof BytesTh){
			scontent = content.asBytes(null);
		}
		else{
			scontent = content.str().toString().getBytes();
		}


		Headers h = ex.getResponseHeaders();

		for (Thing t : ((ListTh)headers).toJavaList()) {
			RecordTh r = (RecordTh)t;			
			h.add(r.getField("header", null).toString(), 
					r.getField("value", null).toString());
		}

		ex.sendResponseHeaders(Integer.parseInt(status.toString()), scontent.length);

		// We could enable gzip like this... If the other end can handle it
		// GZIPOutputStream os = new GZIPOutputStream(ex.getResponseBody());

		OutputStream os = ex.getResponseBody();

		os.write(scontent);
		os.close();
	}

	public String toString(){
		String result = method + " " + path;
		if(query != null)
			result += "?" + query;
		return result;
	}
}
