
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.runtime.builtInFun;

import fisher.desugar.DistDesugarer;
import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.BoolTh;
import fisher.runtime.ComponentTh;
import fisher.runtime.RecordTh;
import fisher.runtime.Thing;
import fisher.runtime.dist.ComponentThread;
import fisher.runtime.dist.DistUtil;
import fisher.runtime.dist.Letter;
import fisher.syn.core.Syntax;
import fisher.util.Doom;
import fisher.util.FisherException;


public  class  SyncReplyBIF extends fisher.runtime.BuiltInFunctionTh  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	public static final String name = "syncReply";
	
	public static final String[] responsePacketFieldNames = new String[]{"nonce", "response"};
	
	@Override
	public Thing apply(Thing[] args, Framelike ignoredFrame, Evaller evaller, Syntax src) throws FisherException {
		checkNumberOfArgs(2,3, name, args, evaller, ignoredFrame, src);
		if (args[0] instanceof Letter) {
			Letter letter = (Letter) args[0];
			Thing letCon = letter.contents();
			RecordTh letRec = letCon.asRecord(src);
			
			Thing nonce = letRec.getField(DistDesugarer.nonce, src);
			Thing response = args[1];
			Thing security = args.length == 2 ? null : args[2];
			RecordTh responsePacket = RecordTh.make(src, responsePacketFieldNames, nonce, response);
			DistUtil.send(ComponentTh.mine(src), letter.sender(), responsePacket, security, src);
			return BoolTh.True;
		}
		else {
			Doom.runtime("first argument to "+ name + " must be a letter of some type", src, args[0]);
			return null;
		}
	}

}
