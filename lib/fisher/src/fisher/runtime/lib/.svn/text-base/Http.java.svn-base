package fisher.runtime.lib;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.Authenticator;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.PasswordAuthentication;
import java.net.ProtocolException;
import java.net.URL;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import sun.net.www.protocol.http.AuthCacheImpl;
import sun.net.www.protocol.http.AuthCacheValue;
import fisher.runtime.BoolTh;
import fisher.runtime.IntTh;
import fisher.runtime.RecordTh;
import fisher.runtime.StringTh;
import fisher.runtime.ThingExtended;
import fisher.util.Doom;
import fisher.util.FisherException;

public class Http extends ThingExtended {

	private HttpURLConnection conn;
	private RecordTh headers;
	private String content;
	private String urlparameters;
	private URL uri;
	private String username;
	private String password;

	/**
	 * General (GET, PUT, POST, ...) HTTP request method.
	 */
	public RecordTh request(String method, String uri) throws IOException, FisherException{
		prepareURI(uri);
		prepareRequest(method);
		prepareContentTypeForRequestsWithABody();
		prepareAuthentication();
		writeResponseBody();
		return doRequest();
	}

	public void setCredentials(StringTh username, StringTh password){
		this.username = username.toString();
		this.password = password.toString();
	}

	public void setUrlparameters(RecordTh urlparameters) throws FisherException {
		this.urlparameters = urlify(urlparameters);
	}

	public StringTh getContent() {
		return StringTh.of(content);
	}

	public BoolTh setContent(RecordTh content) throws FisherException {
		this.content = urlify(content);
		return BoolTh.True;
	}

	public BoolTh setContent(StringTh content){
		this.content = content.toString(); 
		return BoolTh.True;
	}

	public BoolTh setHeaders(RecordTh headers) throws FisherException {
		RecordTh headersLoweredKeys = RecordTh.unfinished();
		for (String h : headers.fields.keySet()) {
			headersLoweredKeys.setField(h.toLowerCase(), headers.fields.get(h));
		}
		this.headers = headersLoweredKeys;
		return BoolTh.True;
	}

	public RecordTh getHeaders() {
		return headers;
	}

	private void writeResponseBody() throws IOException {
		if(content != null){
			conn.setDoOutput(true);
			DataOutputStream out = new DataOutputStream(conn.getOutputStream());
			//out.writeBytes(content);
			out.write(content.getBytes());
			out.flush();
			out.close();
		}
	}

	private void prepareContentTypeForRequestsWithABody() {
		// the default
		if(content != null){
			String contentType = "application/x-www-form-urlencoded";
			if(headers != null){
				Object content_type = headers.fields.get("content-type");
				if(content_type != null)
					contentType = content_type.toString();
			}
			conn.setRequestProperty("content-type", contentType);
		}
		// else, GET, HEAD, DELETE ...
	}
	
	private void prepareAuthentication() {
		// http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6626700
		// The credentials from earlier requests are cached, and used if 
		// the current ones doesn't work!!!
		AuthCacheValue.setAuthCache(new AuthCacheImpl());
		if(username != null && password != null){
			Authenticator.setDefault (new HTTPBasicAuthenticator(username, password));
		}
		// remove the current entry
		else{
			Authenticator.setDefault(null);
		}
		
	}

	private void prepareURI(String uri) throws MalformedURLException {
		if(urlparameters != null){
			if(!uri.endsWith("/"))
				uri += "/";
			uri = uri + "?" + urlparameters;
		}
		this.uri = new URL(uri);
	}

	private void prepareRequest(String method) throws IOException, FisherException {
		try {
			conn = (HttpURLConnection) uri.openConnection();
			
			// Set method: GET, POST,...
			conn.setRequestMethod(method);

			// Set Headers
			if (headers != null) {
				for (String key : headers.fields.keySet()) {
					conn.setRequestProperty(key, headers.getField(key, null)
							.asString(null));
				}
			}
		}	
		catch(ProtocolException e){
			System.err.println("ProtocolException: " + e.getMessage());
		}
	}

	private RecordTh doRequest() throws IOException, FisherException {
			StringBuffer content = new StringBuffer();
			
			int status_code = conn.getResponseCode();
			
			if(status_code < 200 || status_code >= 300){
				content.append(conn.getResponseMessage());
			}
			else{
				BufferedReader in = new BufferedReader(new InputStreamReader(conn
						.getInputStream()));
				
				String inputLine;
				while ((inputLine = in.readLine()) != null) {
					content.append(inputLine + "\n");
				}
				in.close();
			}
						
			Map<String, List<String>> requestHeaders = conn.getHeaderFields();
			RecordTh response = createResponseToThorn(content.toString(), status_code, requestHeaders);

			return response;
	}
	
	private RecordTh createResponseToThorn(String content, int status_code, Map<String, List<String>> request_headers) throws FisherException{
		RecordTh response = RecordTh.unfinished();

		// Body
		response.setField("content", StringTh.of(content));

		// Status code
		response.setField("status", IntTh.of(status_code));

		RecordTh headers = RecordTh.unfinished();
		for (String h : request_headers.keySet()) {
			if (h == null)
				continue; // null=[HTTP/1.1 200 OK]
			// Why is headers a list? Not in this lib at the moment.
			headers.setField(h.toLowerCase(), StringTh.of(request_headers.get(h)
					.get(0)));
		}
		headers.finish();
		response.setField("headers", headers);
		response.finish();
		
		return response;
	}

	/**
	 * URLencode the record.
	 * {: s: "ø":} -> s=%F8 
	 */
	private String urlify(RecordTh urlparameters) throws FisherException{
		try{
			ArrayList<String> kvpairs = new ArrayList<String>();
			RecordTh rec = (RecordTh)urlparameters;
			for(String key : rec.fields.keySet()){
				kvpairs.add(key + "=" + URLEncoder.encode(rec.fields.get(key).toString(), "UTF-8"));
			}
			// '&'.join(kvpairs) in java
			Iterator<String> it = kvpairs.iterator();
			StringBuffer res = new StringBuffer(it.next());
			while(it.hasNext()){
				res.append("&").append(it.next());
			}
			return res.toString();
		}
		catch(UnsupportedEncodingException e){
			System.err.println("UnsupportedEncodingException: " + e.getMessage());
			Doom.runtime(e.getMessage(), null);
			return "";
		}
	}

	class HTTPBasicAuthenticator extends Authenticator {  
		
		private String username;
		private String password;
		
		public HTTPBasicAuthenticator(String username, String password){
			super();
			this.username = username;
			this.password = password;
		}
		
		protected PasswordAuthentication getPasswordAuthentication() {
			return new PasswordAuthentication(username, password.toCharArray());
	  }
	}

}