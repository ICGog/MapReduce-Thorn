
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

import fisher.eval.Evaller;
import fisher.runtime.StringTh;
import fisher.runtime.Thing;
import fisher.runtime.dist.ComponentThread;
import fisher.runtime.dist.ConnectionThread;
import fisher.runtime.dist.SiteData;
import fisher.syn.core.Syntax;
import fisher.test.TestUtils;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  StringSocketThread extends Thread  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public final ComponentThread messagesGoTo;
	//public final Pattern endOfStringPattern;
	public final Socket socket;
	public final Thing protocol;
	
	protected BufferedReader in;
	protected final SiteData siteData;

	//	private StringBuffer sb;
	public StringSocketThread(ComponentThread messagesGoTo,  Socket socket, Thing protocol) {
		super();
		this.messagesGoTo = messagesGoTo;
		//this.endOfStringPattern = endOfStringPattern;
		this.socket = socket;
		this.siteData = messagesGoTo.siteData;
		this.protocol = protocol;
	}

	public void run() {
		boolean goon = true;
		InputStream inputStream;
		Syntax src = messagesGoTo.evaller.wholeSrc;
		try {
			//			socket.setSoTimeout(ConnectionThread.SOCKET_TIMEOUT);
			inputStream = socket.getInputStream();
			TestUtils.say(TestUtils.stringSocketRecv, this, "STRING RECV on : " + inputStream + " localPort= " + socket.getLocalPort() 
					+ " port=" + socket.getPort() +
					" src=" + src + "");
			in = new BufferedReader(new InputStreamReader(inputStream));
			BufferedWriter out = new java.io.BufferedWriter(new java.io.OutputStreamWriter(socket.getOutputStream()));
			ReplySocketTh sr = new ReplySocketTh(out, socket);

			while (goon && !siteData.allComponentsDone()) {
				TestUtils.say(TestUtils.stringSocketRecv, this, "STRING RECV head of loop : " + inputStream + " src=" +src);
				String line = in.readLine();
				
				TestUtils.say(TestUtils.stringSocketRecv, this, "STRING RECV **GOT IT** : " + line + "   src=" + src);

				if (line != null) {
					MsgComp2StringSocket msg = new MsgComp2StringSocket(sr, StringTh.of(line), protocol, null);
					messagesGoTo.mailbox.putLetterInQueue(msg);
				}
			}
			TestUtils.say(TestUtils.stringSocketRecv, this, "STRING RECV after loop : " + inputStream + 
					" src=" + src +
					" goon=" + goon + " !siteData.allComponentsDone()=" + (!siteData.allComponentsDone()));
		} catch (SocketException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
