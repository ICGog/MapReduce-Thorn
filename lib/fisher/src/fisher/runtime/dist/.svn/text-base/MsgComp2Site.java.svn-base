
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.runtime.dist;

import fisher.runtime.ComponentTh;
import fisher.runtime.SiteTh;
import fisher.runtime.Thing;
import fisher.runtime.auxil.ImmutabilityContext;

public  class  MsgComp2Site extends LetterWithSerializedContents  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public final ComponentTh sender;
	public final SiteTh receiver;
	public MsgComp2Site(ComponentTh sender, SiteTh receiver, Thing contents, Thing securityInfo) {
		super(contents, securityInfo);
		this.sender = sender;
		this.receiver = receiver;
	}
	public String toString() {
		return "Msg{" + contents() + " from " + sender + " to " + receiver + "}";
	}
	public String toStringWithoutContents() {
		return "Msg{ (unopened) " +  " from " + sender + " to " + receiver + "}";
	}
	public Thing sender() {return sender;} 
	@Override
	public String typeString() {
		return "msgComp2Site";
	}
	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		return true;
	}
	
}
