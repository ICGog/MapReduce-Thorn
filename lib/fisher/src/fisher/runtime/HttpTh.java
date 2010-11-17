package fisher.runtime;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.HashMap;
import java.util.Map;

import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.runtime.lib.Http;
import fisher.syn.core.Syntax;
import fisher.util.Doom;
import fisher.util.FisherException;

public class HttpTh extends ThingImmutable {

	private static final int HEADERS = 0;
	private static final int GET = 1;
	private static final int POST = 2;
	private static final int CONTENT = 3;
	private static final int PUT = 4;
	private static final int HEAD = 5;
	private static final int DELETE = 6;
	private static final int CREDENTIALS = 7;
	
	private static Map<String, Integer> methodCode = new HashMap<String, Integer>();
	static {
		methodCode.put("headers", HEADERS);
		methodCode.put("get", GET);
		methodCode.put("post", POST);
		methodCode.put("put", PUT);
		methodCode.put("content", CONTENT);
		methodCode.put("head", HEAD);
		methodCode.put("delete", DELETE);
		methodCode.put("credentials", CREDENTIALS);
	}

	private Http http;
	
	public HttpTh() {
		http = new Http();
	}

	@Override
	public Thing invokeMethod(String methodName, Thing[] args,  Syntax src) throws FisherException {
		Evaller evaller = Evaller.mine();
		Framelike frame = this.frameForInvoke();
		if (methodCode.containsKey(methodName)) {
			Integer methodC = methodCode.get(methodName);
			switch (methodC) {
			case HEADERS:
				return this.headers(methodName, args, evaller, frame, src);
			case GET:
				return this.request("GET", methodName, args, evaller, frame, src);
			case POST:
				return this.request("POST", methodName, args, evaller, frame, src);
			case PUT:
				return this.request("PUT", methodName, args, evaller, frame, src);
			case HEAD:
				return this.request("HEAD", methodName, args, evaller, frame, src);
			case DELETE:
				return this.request("DELETE", methodName, args, evaller, frame, src);
			case CONTENT:
				return this.content(methodName, args, evaller, frame, src);
			case CREDENTIALS:
				return this.credentials(methodName, args, evaller, frame, src);
			default:
				break;
			}
		}
		return super.invokeMethod(methodName, args,  src);
	}
	
	private Thing credentials(String methodName, Thing[] args, Evaller evaller,
			Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		checkArg(args[0], StringTh.class, null);
		checkArg(args[1], StringTh.class, null);
		http.setCredentials((StringTh)args[0], (StringTh)args[1]);
		return NULL;
	}

	/**
	 * Get or set http headers.
	 */
	private Thing headers(String methodName, Thing[] args, Evaller evaller,
			Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 1, methodName, args, evaller, frame, src);
		if(args.length == 0)
			return http.getHeaders();
		else{
			// FIXME: how much checking of args?
			checkArg(args[0], RecordTh.class, null);
			return http.setHeaders((RecordTh)args[0]);
			}
	}

	/**
	 * Do a request.
	 */
	protected Thing request(String httpmethod, String methodName, Thing[] args, Evaller evaller,
			Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(1, 2, methodName, args, evaller, frame, src);
		checkArg(args[0], StringTh.class, null);
		try{
			if(args.length == 2){
				checkArg(args[1], RecordTh.class, null);
				http.setUrlparameters((RecordTh)args[1]);
			}
			return http.request(httpmethod, args[0].asString(null));
		}
		catch(MalformedURLException e){
			return Doom.runtime(e.getMessage(), null);
		}
		// FIXME: We probably want to parse on the exception to Thorn...
		catch(IOException e){
			return Doom.runtime(e.getMessage(), null);
		}
	}
	
	/**
	 * Get or set the content of the request.
	 */
	protected Thing content(String methodName, Thing[] args, Evaller evaller,
			Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 1, methodName, args, evaller, frame, src);
		if(args.length == 0)
			return http.getContent();
		else{
			if (args[0] instanceof RecordTh){
				return http.setContent((RecordTh)args[0]);
			}
			else{
				checkArg(args[0], StringTh.class, null);
				return http.setContent((StringTh)args[0]);
			}
		}
	}

	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		return false;
	}

	@Override
	public String typeString() {
		return "http";
	}
}
