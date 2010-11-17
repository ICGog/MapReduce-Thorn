
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
import java.net.*;
import java.util.regex.Pattern;

import fisher.runtime.*;
import fisher.runtime.dist.*;
import fisher.runtime.lib.sox.NetSite;
import fisher.test.TestUtils;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  StringSocketeer extends ThingExtended implements Runnable  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public final int port;
	public final ComponentThread messagesGoTo;
	public ServerSocket serverSocket;
	public final SiteData site;
	
	
	
	public StringSocketeer(Thing port) throws FisherException {
		this((int)port.asJavaInt(null), ComponentThread.mine(), "");
		Thread itsThread = new Thread(this);
		itsThread.start();
	}

	public StringSocketeer(int port, ComponentThread messagesGoTo, String endOfStringRegexp) {
		super();
		this.port = port;
		this.messagesGoTo = messagesGoTo;
		this.site = messagesGoTo.siteData;
	}

	@Override
	public String typeString() {
		return "StringSocketeer";
	}
	
	public Thing protocol() {return null;}

	public void run() {
		try {
			serverSocket = ConnectionAcceptingThread.acquireServerSocket(port, 0, ConnectionAcceptingThread.backoffs);
			serverSocket.setSoTimeout(ConnectionAcceptingThread.ServerSocketTimeout);
			while (!site.allComponentsDone()) {
				try {
					TestUtils.say(false, this, "Head of loop");
					Socket newConnection = serverSocket.accept();
					StringSocketThread sst = createSuitableSocketThread(newConnection);
					sst.start();
				} catch (IOException e) {
					// Normal Timeout
				}
			}// while
		} catch (SocketException e) {
			System.err.println("Oh, dear, in StringSocketeer");
			e.printStackTrace();
		} finally {
			try {
				//whinge("ConnectionAcceptingThread -- trying to kill socket for " + site);
				serverSocket.close();
				//whinge("OK, closed it.");
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	protected StringSocketThread createSuitableSocketThread(Socket newConnection) {
		return new StringSocketThread(messagesGoTo,  newConnection, this.protocol());
	}

}
