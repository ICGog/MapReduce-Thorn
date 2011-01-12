
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

import fisher.runtime.StringTh;
import fisher.runtime.Thing;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.runtime.dist.AbstractLetter;
import fisher.runtime.dist.Letter;
import fisher.runtime.lib.socketeer.ReplySocketTh;
import fisher.util.FisherException;

public  class  MsgComp2HTTPSocket extends AbstractLetter implements Letter  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	public final ReplySocketTh replySocket;
	public final HTTPMessage request;
	private final Thing securityInfo; 

	public MsgComp2HTTPSocket(ReplySocketTh replySocket, HTTPMessage request, Thing securityInfo) {
		super();
		this.replySocket = replySocket;
		this.request = request;
		this.securityInfo = securityInfo;
	}

	public Thing securityInfo() {return securityInfo;}
	
	public Thing contents() throws FisherException {
		// TODO Auto-generated method stub
		return request;
	}

	public Thing sender() {
		// TODO Auto-generated method stub
		return replySocket;
	}

	public String toStringWithoutContents() {
		return "http-msg";
	}

	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		return false;
	}

	@Override
	public String typeString() {
		return "MsgComp2HTTPSocket";
	}

}
