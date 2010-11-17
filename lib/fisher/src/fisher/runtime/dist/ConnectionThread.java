
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
/**
 * 
 */
package fisher.runtime.dist;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InterruptedIOException;
import java.io.ObjectInputStream;
import java.net.Socket;
import java.net.SocketTimeoutException;

import fisher.test.TestUtils;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  ConnectionThread extends Thread  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public static int SOCKET_TIMEOUT = 1000;
	SiteData siteData;
	Socket socket;

	public ConnectionThread(SiteData site, Socket socket) {
		super();
		this.siteData = site;
		this.socket = socket;
	}

	private void whinge(Object... stuff) {
		TestUtils.say(TestUtils.connections, this, Bard.sep(stuff, " "), " -- " + siteData);
	}

	public void run() {
		boolean goon = true;
		try {
			socket.setSoTimeout(SOCKET_TIMEOUT);
			InputStream inputStream = socket.getInputStream();
			ObjectInputStream isr = new ObjectInputStream(inputStream);
			while (goon && ! siteData.allComponentsDone()) {
				try {
					try {
						//whinge("About to try to read an object.");
						Object o = isr.readObject();
						// Can't print 'o' from this thread if it has an object in it: 
						whinge("I read something which I must not print");
						if (o instanceof MsgComp2Comp) {
							MsgComp2Comp intersite = (MsgComp2Comp) o;
							// TODO -- this should probably be in a separate thread.
							siteData.deliverMsgToMailbox(intersite);
						}
						else if (o instanceof MsgComp2Site) {
							MsgComp2Site toSite = (MsgComp2Site) o;
							assert(toSite.receiver.equals(siteData.siteTh));
							siteData.deliverMsgToSecretary(toSite);
							
						}
					} catch (SocketTimeoutException timeoutex) {
						// This is OK, just a timeout on reading from the channel.
						// It means that nobody sent anything recently.
						// That's fine -- but we do need to go back to the while loop
						// and check goOn.
					}
				} catch (ClassNotFoundException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
			//				System.out.println("Mailman: dying!");
		} catch (InterruptedIOException oops) {
			// This is part of normal termination
			//				System.out.println("Mailman - dying normally.");
		} catch (EOFException eofe) {
			goon = false;
			try {
				socket.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
		}
		catch (IOException e) {
			e.printStackTrace();
		}
	}
}
