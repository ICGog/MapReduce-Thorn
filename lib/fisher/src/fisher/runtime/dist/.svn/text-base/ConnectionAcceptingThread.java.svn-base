
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
import java.io.InputStreamReader;
import java.io.InterruptedIOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;

import fisher.test.TestUtils;
import fisher.util.Bard;
import fisher.util.SafeBufferedReader;

public  class  ConnectionAcceptingThread extends Thread  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public static final int ServerSocketTimeout = 300;
	public final SiteData site;
	public ServerSocket serverSocket = null;
	
	public ConnectionAcceptingThread(SiteData site, ServerSocket serverSocket) {
		this.site = site;
		this.serverSocket = serverSocket;
		this.setName("ConnectionAcceptingThread");
	}

	public ConnectionAcceptingThread(SiteData site) {
		this(site, null);
	}

	public String toString() {
		return "ConnectionAcceptingThread{" + site + "}";
	}

	private void whinge(Object... stuff) {
		TestUtils.say(TestUtils.printConnAccThr, this, Bard.sep(stuff, " "));
	}

	// See 'acquireServerSocket'
	public static int[] backoffs = new int[] { 10, 30, 100, 300, 1000 };

	@Override
	public void run() {
		try {
			if (serverSocket == null) serverSocket = acquireServerSocket(site.port(), 0, backoffs);

			// We want to be able to interrupt this thread and have it die.
			// That is done by setting site.goon to false, and
			// interrupting <code>this</code>. But we seem to need a timeout...
			// And the timeout seemes to need to be pretty small, which disturbs
			// me.
			// TODO -- figure out how to do this right.
			// Later: I think the issue is that I'm trying to create lots of
			// nodes with the same port in quick succession,
			// which is useful for testing but I guess not so common in regular
			// life.
			// I have redone the testFun to pick new ports every time, which
			// seems to deal with the issue.
			if (serverSocket == null) {
				System.err.println("ConnectionAcceptingThread: null server socket");
				RuntimeException re = new RuntimeException("Aieee1!");
				re.printStackTrace();
			}
			serverSocket.setSoTimeout(ConnectionAcceptingThread.ServerSocketTimeout);
			while (! site.allComponentsDone() && !this.interrupted()) {
				TestUtils.say(TestUtils.printConnAccThr, this, "head of site.allDone loop in run()");
				try {
					Socket newConnection = serverSocket.accept();
					ConnectionThread ct = new ConnectionThread(site,
							newConnection);
					ct.start();
				} catch (InterruptedIOException e) {
					// The accept() timed out.
					whinge("Normal timeout");
				} catch(Exception ex) {
					System.err.println("ConnectionAcceptingThread -- woof!" + ex);
				}
			}
		} catch (Exception ex) {
			System.err.println("Oh, dear, in ConnectionAcceptingThread");
			ex.printStackTrace();
		} finally {
			try {
				 whinge("ConnectionAcceptingThread -- trying to kill socket for " + site);
				serverSocket.close();
				 whinge("OK, closed it.");
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	public static ServerSocket acquireServerSocket(int port, int pos, int[] backoffs) {
		/*
		 * Early on, we had transient trouble with 'connection refused' errors.
		 * So I am trying an exponential backoff approach. If there's an error,
		 * we try a few times with increasing delays. The array <code>backoffs</code>
		 * gives the delays. The numbers are chosen without any particular
		 * cleverness.
		 */
		
		ServerSocket attemptedServerSocket = null;
		try {
//			whinge("Starting search for " + site.port() + ", try " + (pos + 1) + " for siteData=" + site 
//					+ " with secretary = " + site.secretary.component
//			);
			attemptedServerSocket = new ServerSocket(port);
			TestUtils.serverSockets.add(attemptedServerSocket);
//			whinge("Got " + site.port());
			return attemptedServerSocket;
		} catch (IOException e) {
			if (pos < backoffs.length) {
				try {
					sleep(backoffs[pos]);
				} catch (InterruptedException e1) {
					// Interruption means, time for this thread to quit!
					return null;
				}
				acquireServerSocket(port, pos + 1, backoffs);
			}
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}
	
}
