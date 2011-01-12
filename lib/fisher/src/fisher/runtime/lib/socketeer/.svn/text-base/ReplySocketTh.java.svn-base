
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.runtime.lib.socketeer;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Serializable;
import java.io.Writer;
import java.net.Socket;

import fisher.runtime.BoolTh;
import fisher.runtime.StringTh;
import fisher.runtime.Thing;
import fisher.runtime.ThingExtended;
import fisher.util.FisherException;

public  class  ReplySocketTh extends ThingExtended   { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public final Writer out;
	public final Socket socket;
	
	
	public ReplySocketTh(Writer out, Socket socket) {
		super();
		this.out = out;
		this.socket = socket;
	}
	
	@Override
	public String typeString() {
		return "ReplySocket";
	}
	
	public Thing send(Thing thing, Thing ignoredSecurity) throws FisherException{
		try {
			String s = thing.toString();
			out.append(s);
			out.flush();
			return BoolTh.True;
		} catch (IOException e) {
			return BoolTh.False;
		}
	}
	
	public String toString() {
		return socket.toString();
	}
	
}
