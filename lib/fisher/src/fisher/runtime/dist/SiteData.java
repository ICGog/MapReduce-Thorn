
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

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.ComponentTh;
import fisher.runtime.SiteTh;
import fisher.runtime.Thing;
import fisher.runtime.lib.socketeer.StringSocketThread;
import fisher.runtime.lib.sox.AnythingSocketThread;
import fisher.runtime.lib.sox.SoxUtil;
import fisher.syn.core.Syntax;
import fisher.syn.interfaces.ComponentInfo;
import fisher.test.TestUtils;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  SiteData  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public final SiteTh siteTh;
	public Doorman doorman;
	public final ConnectionAcceptingThread connectionAcceptingThread;
	public ComponentThread secretary;
	private List<ComponentThread> subsidiaryThreads = (new ArrayList<ComponentThread>());
	private Map<ComponentTh, ComponentThread> components = new HashMap<ComponentTh, ComponentThread>();
	private long componentCounter = 0;

	private Map<SiteTh, ObjectOutputStream> sendingStreams = new HashMap<SiteTh, ObjectOutputStream>();
	private Map<SiteTh, Socket> sendingSockets = new HashMap<SiteTh, Socket>();
	private Map<SiteTh, Writer> sendingWriters = new HashMap<SiteTh, Writer>();

	private static ThreadLocal<SiteData> siteDataPerThread = new ThreadLocal<SiteData>();

	public static void register(SiteData sd) {
		assert (siteDataPerThread.get() == null);
		siteDataPerThread.set(sd);
	}

	public static SiteData mine() {
		SiteData sd = siteDataPerThread.get();
		if (sd == null) {
			assert (sd != null);
		}
		return sd;
	}

	public static void reset() {
		siteDataPerThread = new ThreadLocal<SiteData>();
	}

	@Deprecated
	public static SiteData start(SiteTh site, ComponentInfo src, Framelike surroundingFrame) throws FisherException {
		return new SiteData(site, src, surroundingFrame);
	}
	
	
	public static SiteData start(SiteTh site, ComponentInfo src, Framelike surroundingFrame, ServerSocket serverSocket) throws FisherException {
		return new SiteData(site, src, surroundingFrame, serverSocket);
	}
	
	

	private SiteData(SiteTh site, ComponentInfo src, Framelike surroundingFrame) throws FisherException {
		this(site, src, surroundingFrame, null);
	}
	private SiteData(SiteTh site, ComponentInfo src, Framelike surroundingFrame, ServerSocket serverSocket) throws FisherException {
		super();
		this.siteTh = site;
		this.doorman = new Doorman(this);
		this.secretary = this.spawn(src, surroundingFrame);
		this.connectionAcceptingThread = new ConnectionAcceptingThread(this, serverSocket);
		this.connectionAcceptingThread.start();
	}

	public ComponentThread spawn(ComponentInfo src, Framelike frame) throws FisherException {
		ComponentThread ct = ComponentThread.start(this, src, frame);
		return ct;
	}

	public int port() {
		return siteTh.port;
	}

	public String toString() {
		return "SiteImpl{" + siteTh + "}";
	}

	public synchronized void noteNewComponent(ComponentThread thr) {
		subsidiaryThreads.add(thr);
		components.put(thr.compTh, thr);
		TestUtils.say(TestUtils.printNewComponent, this, "New component: ", thr.compTh, " viz. ", thr);
	}

	public ComponentTh newCompTh(ComponentInfo compInfo) {
		String name = (compInfo.name() == null ? "comp-" : (compInfo.name() + "-")) + componentCounter;
		return ComponentTh.of(this.siteTh, anotherComponentNumber(), name);
	}

	public long anotherComponentNumber() {

		return (componentCounter++);
	}

	public synchronized boolean allComponentsDone() {
		for (ComponentThread ct : subsidiaryThreads) {
			if (!ct.finished)
				return false;
		}
		return true;
	}

	public synchronized String unfinishedThreadNames() {
		StringBuffer sb = new StringBuffer();
		for (ComponentThread ct : subsidiaryThreads) {
			if (!ct.finished)
				sb.append(ct.toString() + " ");
		}
		if (doorman.isAlive()) {sb.append(" doorman ");}
		if (connectionAcceptingThread.isAlive()) {sb.append(" connAccThr ");}
		return sb.toString();
	}
	
	public synchronized boolean allDoneAltogether() {
		if (!allComponentsDone())
			return false;
		return !doorman.isAlive() && !connectionAcceptingThread.isAlive();
	}

	//////////////////////////////////////////////
	// Methods for receiving messages 
	//////////////////////////////////////////////

	public synchronized void deliverMsgToMailbox(MsgComp2Comp msg) {
		ComponentTh to = msg.receiver;
		if (components.containsKey(to)) {
			ComponentThread toThread = components.get(to);
			toThread.mailbox.putLetterInQueue(msg);
		} else {
			System.err.println(this + ": Can't deliver message to " + to);
			// I dunno what else to do with it.
		}
	}

	public synchronized void deliverMsgToSecretary(MsgComp2Site msg) {
		ComponentThread toThread = this.secretary;
		toThread.mailbox.putLetterInQueue(msg);
	}

	//////////////////////////////////////////////
	// Methods for sending messages 
	//////////////////////////////////////////////

	private static int[] backoffs = new int[] { 10, 30, 100,  300 };

	public synchronized void loseSendStreamTo(SiteTh to, Syntax src) throws FisherException {
		sendingSockets.remove(to);
		sendingStreams.remove(to);
		sendingWriters.remove(to);
	}

	/**
	 * This requires that acquireSendStreamTo has been called on this site
	 * first.
	 * 
	 * @param to
	 * @param src
	 * @return
	 * @throws FisherException
	 */
	public synchronized Socket getSocketTo(SiteTh to, Syntax src) throws FisherException {
		Socket socket = sendingSockets.get(to);
		if (socket == null) {
			Doom.internal("No socket for " + to + "; perhaps acquireSendStreamTo has not been called", src);
		}
		return socket;
	}

	public Writer getOrMakeWriterTo(SiteTh to, Thing protocol, Syntax src) throws FisherException {
		// This is done kind of lazily -- we usually don't need writers.
		if (sendingWriters.containsKey(to))
			return sendingWriters.get(to);
		// And only certain sites allow writers.
		if (!to.openStreamsAsWriter()) {
			Doom.runtime("This is not the kind of site that can have strings sent to it.", src);
		}
		synchronized (this) {
			if (!sendingSockets.containsKey(to)) {
				acquireSendStreamTo(to, src);
			}
			try {
				Socket socket = getSocketTo(to, src);
				final OutputStream outputStream = socket.getOutputStream();
				BufferedWriter out = new java.io.BufferedWriter(new java.io.OutputStreamWriter(outputStream));
				sendingWriters.put(to, out);
				// And start a thread to listen to messages on that socket, so the other end can reply
				String inputStyle = SoxUtil.inputStyle(protocol);
				AnythingSocketThread getAnswers = new AnythingSocketThread(ComponentThread.mine(),socket, protocol, inputStyle);
				getAnswers.start();
				return out;
			} catch (IOException e) {
				Doom.runtime("Can't get socket: " + e, src, e, to);
				return null;
			}
		}
	}

	public synchronized ObjectOutputStream acquireSendStreamTo(SiteTh to, Syntax src) throws FisherException {
		
		if (sendingSockets.containsKey(to)) {
			TestUtils.say(TestUtils.printSend, this, "acquireSendStreamTo: from cache got stream to ", to);
			Socket socket = sendingSockets.get(to);
			if (!socket.isClosed() && !socket.isInputShutdown() && !socket.isOutputShutdown()) {
				ObjectOutputStream ss = sendingStreams.get(to);
				return ss;
			}
		}
		for (int b : backoffs) {
			boolean last = (b == backoffs[backoffs.length - 1]);
			/*
			 * We make several tries to get the socket. This is mainly important for testing --
			 * when we start and end many sites quickly, using the same port numbers. 
			 */
			try {
				
				TestUtils.say(TestUtils.http, this, "Acquiring socket at:" + to.nodeName + ":" + to.port);
				// Not cached.  Let's get it.
				Socket socket = new Socket(to.nodeName, to.port);
				TestUtils.say(TestUtils.http, this, "Getting a new socket: " + socket, "b=" + b);
				
				sendingSockets.put(to, socket);
				
				if (to.openStreamsAsObjectOutputStream()) {
					ObjectOutputStream sendStream = new ObjectOutputStream(socket.getOutputStream());
					sendingStreams.put(to, sendStream); // cache it
					TestUtils.say(TestUtils.printSend, this, "acquireSendStreamTo: newly got stream to ", to);
					return sendStream;
				}
				else {
					return null; // need to break the loop.
				}
			} catch (UnknownHostException e) {
				if (last) {
					Doom.runtime("Unknown host: " + to.nodeName, src, to, e);
					return null;
				}

			} catch (IOException e) {
//				TestUtils.say(true, this, "ioe:" + e);
				if (last) {
					Doom.runtime(e.getMessage(), src, to, e);
					return null;
				}
				
			}
			TestUtils.say(TestUtils.printAcquireSendStreamTo, this, "Backing off " + b + " acquiring stream to " + to);
			try {
				Thread.sleep(b);
			} catch (Exception e) {
				// woke up early -- go to work anyways.
			}
		}
		return null; // never reached.
	}

}
