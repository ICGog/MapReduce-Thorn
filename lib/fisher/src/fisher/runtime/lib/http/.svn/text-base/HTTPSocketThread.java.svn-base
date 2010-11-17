
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

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.InterruptedIOException;
import java.io.ObjectInputStream;
import java.io.Reader;
import java.net.Socket;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.util.regex.Pattern;

import fisher.runtime.StringTh;
import fisher.runtime.dist.ComponentThread;
import fisher.runtime.dist.ConnectionThread;
import fisher.runtime.dist.SiteData;
import fisher.runtime.lib.socketeer.ReplySocketTh;
import fisher.test.TestUtils;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  HTTPSocketThread extends Thread  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public final ComponentThread messagesGoTo;
	//public final Pattern endOfStringPattern;
	public final Socket socket;
	private InputStream inputStream;
	private BufferedReader in;
	private final SiteData siteData;

	//	private StringBuffer sb;
	public HTTPSocketThread(ComponentThread messagesGoTo,  Socket socket) {
		super();
		this.messagesGoTo = messagesGoTo;
		//this.endOfStringPattern = endOfStringPattern;
		this.socket = socket;
		this.siteData = messagesGoTo.siteData;
		this.setName("HTTPSocket/"+ messagesGoTo.getName());
	}

	public void run() {
		boolean goon = true;
		try {
			//			socket.setSoTimeout(ConnectionThread.SOCKET_TIMEOUT);
			inputStream = socket.getInputStream();
			in = new BufferedReader(new InputStreamReader(inputStream));
			BufferedWriter out = new java.io.BufferedWriter(new java.io.OutputStreamWriter(socket.getOutputStream()));
			ReplySocketTh sr = new ReplySocketTh(out, socket);
			int sleepage = 10;
			while (goon && !siteData.allComponentsDone()) {
//				THIS is where the HTTPification goes.
//				String line = in.readLine();
				HTTPMessage request = HTTPRequest.read(in, sr);
				if (request != null) {
					MsgComp2HTTPSocket msg = new MsgComp2HTTPSocket(sr, request, null);
					messagesGoTo.mailbox.putLetterInQueue(msg);
				}
				else {
					/* Sometimes this code would go into a tight loop here and eat the whole CPU.
					 * (It would happen if wget were connecting to it, though not Firefox)
					 * So, if the request is null, wait a little while before looking again.
					 * Start off with 10 ms, and increase it slowly to 0.5 sec.  
					 * TODO: This is a complete hack based on me not really knowing what's going on,
					 * so if you know better, do the right thing.
					 * 
					 */
					try {
						Thread.sleep(sleepage);
					} catch (InterruptedException e) {
						// interrupted, which is fine.
					}
					sleepage = Math.min(sleepage + 10, 500); // wait a little while.
					
				}
			}
		} catch (SocketException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public String toString() {
		return "a HTTPSocketThread";
	}
	
}
