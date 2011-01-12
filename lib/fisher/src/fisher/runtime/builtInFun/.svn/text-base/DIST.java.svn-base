
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
import fisher.runtime.IntTh;
import fisher.runtime.RecordTh;
import fisher.runtime.SiteTh;
import fisher.runtime.StringTh;
import fisher.runtime.Thing;
import fisher.runtime.dist.ComponentThread;
import fisher.runtime.dist.DistUtil;
import fisher.runtime.dist.Letter;
import fisher.syn.core.Syntax;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  DIST  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public static Thing thisComp() {
		return ComponentThread.mine().compTh;
	}

	public static Thing thisSite() {
		return ComponentThread.mine().siteData.siteTh;
	}

	public static Thing site(Thing ipth, Thing portth) throws FisherException {
		final Syntax lastSyntax = Evaller.lastSyntax();
		String ip = ipth.asString(lastSyntax);
		int port = portth.asJavaInt(lastSyntax);
		return new SiteTh(ip, port);
	}

	static ThreadLocal<Long> nonceCtr = new ThreadLocal<Long>();

	public static Thing newNonce() throws FisherException {
		Long L = nonceCtr.get();
		if (L == null)
			L = new Long(1);
		nonceCtr.set(L + 1);
		// This is not the least bit secure.  

		return IntTh.of(L);
	}

	public static final String[] responsePacketFieldNames = new String[] { "nonce", "response" };

	public static Thing syncReply(Thing letter0, Thing response) throws FisherException {
		final Syntax src = Evaller.lastSyntax();
		if (letter0 instanceof Letter) {
			Letter letter = (Letter) letter0;
			Thing letCon = letter.contents();

			RecordTh letRec = letCon.asRecord(src);

			Thing nonce = letRec.getField(DistDesugarer.nonce, src);
			RecordTh responsePacket = RecordTh.make(src, responsePacketFieldNames, nonce, response);
			Thing MarcoAndSalNeedToFigureThisOut = null;
			DistUtil.send(ComponentTh.mine(src), letter.sender(), responsePacket, MarcoAndSalNeedToFigureThisOut, src);
			return BoolTh.True;
		} else {
			Doom.runtime("first argument to syncReply must be a letter of some type", src, letter0);
			return null;
		}
	}
	
	public static Thing splitSync() {
		return StringTh.of("splitSync");
	}

}
