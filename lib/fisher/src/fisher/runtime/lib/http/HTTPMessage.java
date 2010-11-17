package fisher.runtime.lib.http;

import java.io.BufferedReader;
import java.io.IOException;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.runtime.IntTh;
import fisher.runtime.ListTh;
import fisher.runtime.RecordTh;
import fisher.runtime.StringTh;
import fisher.runtime.Thing;
import fisher.runtime.ThingExtended;
import fisher.runtime.lib.socketeer.ReplySocketTh;
import fisher.util.Doom;
import fisher.util.FisherException;

public abstract class HTTPMessage extends ThingExtended {
	
	public final StringTh httpProtocolVersion;
	public final StringTh content;
	public final RecordTh allHeaders;
	

	public HTTPMessage(StringTh httpProtocolVersion, StringTh content, RecordTh allHeaders) {
		super();
		this.httpProtocolVersion = httpProtocolVersion;
		this.content = content;
		this.allHeaders = allHeaders;
	}

	public RecordTh headers() {return allHeaders;}
	public Thing content() {return content;}
	public Thing protocolVersion() {return httpProtocolVersion;}
	
	public static HTTPMessage read(BufferedReader in, ReplySocketTh replyGoesTo)  {
		try {
			String firstLine;
			// Skip blank lines 'cause I think that's right.
			do {
				firstLine= in.readLine();
			} while (firstLine == null || firstLine.length() == 0); 
			HTTPMessage msg;
			if (looksLikeRequest(firstLine)) {
				msg = readRequest(firstLine, in, replyGoesTo);
			} else {
				msg = readResponse(firstLine, in, replyGoesTo);
			}
			return msg;
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		} catch (FisherException e) {
			e.printStackTrace();
			return null;
		}
	}

	public static boolean looksLikeRequest(String line) {
		return !(line.startsWith("HTTP"));
	}

	private static final Pattern dissectFirstLine = Pattern.compile("^(.*)([0-9]{3})(.*)$");

	public static HTTPMessage readResponse(String firstLine, BufferedReader in, ReplySocketTh replyGoesTo) throws FisherException {
		String httpProtocolVersion; // HTTP/1.1
		String responseCode; // 200 or 404 or something
		String meaning; // OK or something
		// Dissect the first line, which looks sorta like: 
		//     HTTP/1.1 200 OK
		// I can't find a spec in 18 seconds of looking, so I'm guessing that the three-digit number 
		// is the return code, and the stuff before and after are protocol version and meaning.
		// Which may or may not be right, but is easy to parse.
		final Matcher matcher = dissectFirstLine.matcher(firstLine);
		if (matcher.matches()) {
			httpProtocolVersion = matcher.group(1);
			responseCode = matcher.group(2);
			meaning = matcher.group(3);
		} else {
			Doom.runtimeNonThorn("I can't parse this HTTP header", Evaller.mine().lastSyntax(), firstLine);
			return null;
		}
		
		RecordTh headers = readHeaders(in);
		
		StringTh content = readContent(in, headers);
		
		HTTPMessage resp = new HTTPResponse(
				StringTh.of(httpProtocolVersion),
				StringTh.of(responseCode), // TODO -- should be an integer.
				StringTh.of(meaning),
				headers,
				content
				);
		return resp;
	}

	public static HTTPMessage readRequest(String firstLine, BufferedReader in, ReplySocketTh replyGoesTo) {
		StringTh verb;
		StringTh resource;
		StringTh protocol;
		RecordTh params;
		// RecordTh headers = RecordTh.unfinished();
		try {
			// TODO -- Make the parsing of the first line be right. 
			// I couldn't find a spec in three minutes, so I guessed.
			if (firstLine == null)
				return null;
			StringTh methodLineTh = StringTh.of(firstLine);
			

			String resourceString;

			int firstSpace = firstLine.indexOf(' ');
			final String verbJ = firstSpace >= 0 ? firstLine.substring(0, firstSpace) : firstLine;
			verb = StringTh.of(verbJ);

			int lastSpace = firstLine.lastIndexOf(' ');
			protocol = StringTh.of(lastSpace >= 0 ? firstLine.substring(lastSpace + 1).trim() : "");

			if (firstSpace > 0 && lastSpace > 0) {
				resourceString = firstLine.substring(firstSpace, lastSpace).trim();
				resourceString = URLDecoder.decode(resourceString, "UTF-8");
			} else {
				resourceString = null;
			}

			if (resourceString == null) {
				params = null;
			} else {
				int qmark = resourceString.indexOf("?");
				List<String> paramNames = new ArrayList<String>();
				List<Object> paramValues = new ArrayList<Object>();
				if (qmark > 0) {
					String paramString = resourceString.substring(qmark + 1);
					resourceString = resourceString.substring(0, qmark);
					String[] paramSnips = paramString.split("&");
					for (String paramsnip : paramSnips) {
						int eek = paramsnip.indexOf("=");
						if (eek < 0) {
							paramNames.add(paramsnip);
							paramValues.add(null);
						} else {
							final String name = paramsnip.substring(0, eek);
							paramNames.add(name);
							final String val = paramsnip.substring(eek + 1);
							paramValues.add(val);
						}
					}
				}
				try {
					params = RecordTh.make(Evaller.lastSyntax(), paramNames, paramValues);
				} catch (FisherException e) {
					params = null;
					e.printStackTrace();
				}
			}

			resource = resourceString == null ? null : StringTh.of(resourceString);
			
			RecordTh headers = readHeaders(in);
			
			StringTh content = readContent(in, headers);

			return new HTTPRequest(verb, resource, params, protocol, headers, content, replyGoesTo);

		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return null;
		}
	}

	
	protected static StringTh readContent(BufferedReader in, RecordTh headers) {
		try {
			int length = -1;
			for (Map.Entry<String, Thing> header : headers.fieldSet()) {
				if (header.getKey().toLowerCase().equals("content-length")) {
					length = Integer.parseInt(EvalUtil.toString(header.getValue()).trim());
					break;
				}
			}
			if (length == -1) {
				//Doom.runtimeNonThorn("No content-length field in HTTP message!", Evaller.lastSyntax(), headers);
				length = 0;
			}
			char[] chars = new char[length + 10];
			int nread = in.read(chars, 0, length);
			String sread = new String(chars, 0, nread);
			return StringTh.of(sread);
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}
	}
	
	protected static RecordTh readHeaders(BufferedReader in) {
		//TODO -- I do not know HTTP very well!
		// I have guessed that header lines have the form 
		//    headername:value
		// (with no further encoding or whatever)
		// followed by a blank line.
		RecordTh headers = RecordTh.unfinished();
		
		seekHeaders:while(true) {
			try {
				String line = in.readLine();
				int colon = line.indexOf(':');
				if (colon < 0)
					break seekHeaders;
				String fieldName = line.substring(0, colon);
				String fieldValue = line.substring(colon + 1);
				headers.setField(fieldName.toLowerCase(), StringTh.of(fieldValue));
			} catch (Exception e) {
				e.printStackTrace();
			}
		}//while		
		headers.finish();
		return headers;
	}


	protected static RecordTh buildAllHeaders(StringTh content, RecordTh extraHeaders) throws FisherException {
		RecordTh ah = RecordTh.unfinished();
		ah.setField("Content-length", IntTh.of(content.length()));
		final Map<String, Thing> flds = extraHeaders.fields;
		for (Map.Entry<String,Thing> ent : flds.entrySet()) {
			String k = ent.getKey();
			Thing v = ent.getValue();
			ah.setField(k, v);
		}
		ah.finish();
		return ah;
	}
	
	@Override
	public int hashCode() {
		return  content.hashCode();
	}
	
	public boolean equals(Object other) {
		if (this.getClass() != other.getClass()) return false;
		try {
			HTTPMessage mother = (HTTPMessage) other;
			return EvalUtil.eq(this.content(), mother.content()) && EvalUtil.eq(this.headers(), mother.headers());
		} catch (FisherException e) {
			return false;
		}
	}
	
	public static boolean headerWillBeComputed(String headerName) {
		return headerName.equalsIgnoreCase("content-length");
	}
	

}
