
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

import fisher.eval.EvalUtil;
import fisher.runtime.StringTh;
import fisher.runtime.Thing;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.runtime.dist.AbstractLetter;
import fisher.runtime.dist.Letter;
import fisher.util.Bard;
import fisher.util.FisherException;

public  class  MsgComp2StringSocket extends AbstractLetter implements Letter  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	public final ReplySocketTh replySocket;
	public final StringTh rawContents;
	public final Thing protocol;
	public boolean contentsAreDecoded = false;
	public Thing decodedContents;
	private final Thing securityInfo; 

	public MsgComp2StringSocket(ReplySocketTh replySocket, StringTh string, Thing protocol, Thing securityInfo) {
		super();
		this.replySocket = replySocket;
		this.rawContents = string;
		this.protocol = protocol;
		this.securityInfo = securityInfo;
	}

	public Thing securityInfo() {return securityInfo;}
	
	public void decodeContents() throws FisherException {
		if (contentsAreDecoded) return;
		Thing decoded = protocol == null 
			? rawContents
					:	protocol.invokeMethod("decode", Bard.array(rawContents), null);
		contentsAreDecoded = true;
		decodedContents = decoded;
	}
	
	
	public Thing contents() throws FisherException{
		decodeContents();
		return decodedContents;
	}

	public Thing sender() {
		return replySocket;
	}
	
	public Thing protocol() {
		return protocol;
	}

	public String toStringWithoutContents() {
		return "msg";
	}

	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		return false;
	}

	@Override
	public String typeString() {
		return "MsgComp2StringSocket";
	}

}
