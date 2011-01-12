
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.util;

import java.io.*;

/**
 * @author bard
 * This is from <i>Java Network Programming</i>, p.97.
 * Where it says, "readLine() tends to hang when streams where lines end
 * in carriage returns, as is commonly the case when the streams derive from a
 * Macintosh.  Consequently you should scrupulously avoid this method in
 * network programs"... and then goes on to give this safe alternative.
 */
public  class  SafeBufferedReader extends BufferedReader  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public SafeBufferedReader(Reader in) {
		this(in, 1024);
	}
	
	public SafeBufferedReader(Reader in, int buffersize) {
		super(in, buffersize);
	}
	
	
	private boolean lookingForLineFeed = false;
	
	public String readLine() throws IOException {
		StringBuffer sb = new StringBuffer("");
		while(true){
			int c = this.read();
			if (c == -1) { // end of stream
				if (sb.equals("")) return null;
				return sb.toString();
			}
			else if (c == '\n') {
				if (lookingForLineFeed) {
					lookingForLineFeed = false;
					continue;
				}
				else {
					return sb.toString();
				}
			}
			else if (c == '\r') {
				lookingForLineFeed = true;
				return sb.toString();
			}
			else {
				lookingForLineFeed = false;
				sb.append((char) c);
			}
		}//while
	}
	
}
