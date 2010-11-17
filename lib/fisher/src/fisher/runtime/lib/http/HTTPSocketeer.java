
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

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;

import fisher.runtime.Thing;
import fisher.runtime.ThingExtended;
import fisher.runtime.dist.ComponentThread;
import fisher.runtime.dist.ConnectionAcceptingThread;
import fisher.runtime.dist.SiteData;
import fisher.runtime.lib.socketeer.StringSocketThread;
import fisher.test.TestUtils;
import fisher.util.FisherException;

public  class  HTTPSocketeer extends ThingExtended implements Runnable  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public final int port;
	public final ComponentThread messagesGoTo;
	public ServerSocket serverSocket;
	public final SiteData site;
	
	public HTTPSocketeer(Thing port) throws FisherException {
		this((int)port.asJavaInt(null), ComponentThread.mine(), "");
		Thread itsThread = new Thread(this);
		itsThread.start();
	}

	public HTTPSocketeer(int port, ComponentThread messagesGoTo, String endOfStringRegexp) {
		super();
		this.port = port;
		this.messagesGoTo = messagesGoTo;
		this.site = messagesGoTo.siteData;
	}

	@Override
	public String typeString() {
		return "HTTPSocketeer";
	}

	public void run() {
		try {
			serverSocket = ConnectionAcceptingThread.acquireServerSocket(port, 0, ConnectionAcceptingThread.backoffs);
			serverSocket.setSoTimeout(ConnectionAcceptingThread.ServerSocketTimeout);
			while (!site.allComponentsDone()) {
				try {
					TestUtils.say(false, this, "Head of loop");
					Socket newConnection = serverSocket.accept();
					TestUtils.say(TestUtils.http, this, "Got connection from: " + newConnection);
					HTTPSocketThread sst = new HTTPSocketThread(messagesGoTo,  newConnection);
					sst.start();
				} catch (IOException e) {
					// Normal Timeout
				}
			}// while
		} catch (SocketException e) {
			System.err.println("Oh, dear, in HTTPSocketeer");
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


}
