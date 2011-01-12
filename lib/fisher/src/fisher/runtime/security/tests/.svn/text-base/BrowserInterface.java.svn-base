package fisher.runtime.security.tests;

import java.awt.Desktop;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.runtime.Thing;
import fisher.util.FisherException;

public class BrowserInterface {
	
	private static final boolean DEBUG = true;
	
	private static final String FILE_NAME = "index.html";
	
	public static Thing writeToFile(Thing x) throws FisherException {
		String contents = x.asString(Evaller.lastSyntax());
	    try {
			FileWriter fstream = new FileWriter(FILE_NAME);
		    BufferedWriter out = new BufferedWriter(fstream);
		    StringBuilder sb = new StringBuilder();
		    sb.append("<HTML><BODY>");
		    sb.append(contents);
		    sb.append("</BODY></HTML>");
		    out.write(sb.toString());
		    out.close();
	    } catch (IOException e) {
	    	e.printStackTrace(System.err);
	    	throw new BrowserInterfaceException(e);
	    }
	    File file = new File(FILE_NAME);
	    Thing ret = null;
	    try {
	    	String canonicalPath = file.getCanonicalPath();
	    	ret = EvalUtil.thingify("file://" + canonicalPath);
	    	if (DEBUG) {
	    		System.out.println(ret);
	    	}
	    } catch (IOException e) {
	    	e.printStackTrace(System.err);
	    	throw new BrowserInterfaceException(e);
	    }
	    return ret;
	    // return null;
	}
	
	public static Thing popUpBrowser(Thing x) throws FisherException {
    	if (DEBUG) {
    		System.out.println(x);
    	}
		Desktop desktop = java.awt.Desktop.getDesktop();
		try {
			desktop.browse(java.net.URI.create(x.asString(Evaller.lastSyntax())));
		} catch (IOException e) {
			e.printStackTrace(System.err);
			throw new BrowserInterfaceException(e);
		}
		return null;
	}
	
	private static class BrowserInterfaceException extends FisherException {
		private static final long serialVersionUID = -262788101704472911L;
		
		public BrowserInterfaceException(Throwable e) {
			super(e);
		}
	}
}
