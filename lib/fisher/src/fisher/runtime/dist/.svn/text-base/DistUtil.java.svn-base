
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

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.Writer;
import java.net.Socket;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.runtime.ComponentTh;
import fisher.runtime.ListTh;
import fisher.runtime.SiteTh;
import fisher.runtime.Thing;
import fisher.runtime.security.Security;
import fisher.syn.core.Syntax;
import fisher.test.TestUtils;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  DistUtil  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	private DistUtil() {
	}

	public static void send(ComponentTh from, Thing to, Thing message, Thing security, Syntax src) throws FisherException {
		if (EvalUtil.isIter(to)) {
			for (Thing thing : EvalUtil.iter(to, src)) {
				send1(from, thing, message, security, src);
			}
		} else {
			send1(from, to, message, security, src);
		}
	}

	private static void send1(ComponentTh from, Thing to, Thing message, Thing security, Syntax src) throws FisherException {
		SiteData siteData = SiteData.mine();
		TestUtils.say(TestUtils.printSend, IT, "SEND: " + src +  " msg=" + message) ;
		SiteTh toSite;
		ComponentTh toC = null;
		boolean sendToComponent;
		if (to instanceof ComponentTh) {
			toC = (ComponentTh) to;
			toSite = toC.site;
			sendToComponent = true;
		} else if (to instanceof SiteTh) {
			toSite = (SiteTh) to;
			sendToComponent = false; // site, instead
		} else {
			Doom.runtime("Can only send messages to components and sites, not a " + EvalUtil.kind(to), src, to);
			return; // Actually it throws, not returns.			
		}
		TestUtils.say(TestUtils.printSend, IT, "SEND: " + src + " toSite=" + toSite + " comp?=" + sendToComponent + " msg=" + message + " security=" + security) ;

		Thing securityInfo = Security.secure(message, security);
		
		/*
		 * We do a bit of elementary guarding against streams being closed, or against the peer
		 * going down and getting restarted.  We try to send once.  If that doesn't work, 
		 * we get rid of the connection to that site, acquire another one, and try again.
		 * If the second try fails, too doom.
		 */
//		if (sendToComponent) {
			for (int ntry = 1; ntry <= 2; ntry++) {
				ObjectOutputStream outputStreamToReceiver = siteData.acquireSendStreamTo(toSite, src);
				TestUtils.say(TestUtils.printSend, IT, "SEND: " + src + " toSite=" + toSite + " outputStream=" + outputStreamToReceiver);
				Letter letter = 
					sendToComponent 
						? (Letter) new MsgComp2Comp(from, toC, message, securityInfo)
				 		: (Letter) new MsgComp2Site(from, toSite, message, securityInfo);
				try {
					synchronized (outputStreamToReceiver) {
						outputStreamToReceiver.writeObject(letter);
					}
					break;
				} catch (IOException e) {
					// Probably the peer is down, and another one is up at the same site.
					// First try: forget that socket, and try again
					if (ntry == 1)
						siteData.loseSendStreamTo(toSite, src);
					// Second try: give up.
					else{
						Doom.runtime("Exception writing message: " + e.getMessage(), src, from, to, message, e);
					}
				}
			}
	}
	
	private static final DistUtil IT = new DistUtil();
	
	private static boolean pleaseEndWithNewline(Thing protocol) throws FisherException {
		Thing bo = EvalUtil.getFieldOfObjectTh_or_null("pleaseEndWithNewline", protocol);
		if (bo == null) return true;
		return bo.asBoolean(Evaller.lastSyntax());
	}
	
	public static void sendString(String string, SiteTh to, Thing protocol, Syntax src) throws FisherException {
		SiteData siteData = SiteData.mine();
		siteData.acquireSendStreamTo(to, src);
		Writer writer = siteData.getOrMakeWriterTo(to, protocol, src);
		try {
			TestUtils.say(TestUtils.http, IT, "Just sending " + string + " to " + to );
			writer.append(string);
			if (!(string.endsWith("\n")) && (protocol == null || pleaseEndWithNewline(protocol))) {
				writer.append("\n");
			}
			writer.flush();
			TestUtils.say(TestUtils.http, IT, "Just sent+flushed " + string + " to " + to );
		} catch (IOException e) {
			Doom.runtime("I/O error: " + e, src, " while writing: ", string,  "to=" + to); 
		}
	}
}
