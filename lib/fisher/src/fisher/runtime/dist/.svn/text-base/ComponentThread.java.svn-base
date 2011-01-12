
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

import java.util.List;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;

import fisher.eval.Evaller;
import fisher.eval.Frame;
import fisher.eval.Matchiste;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.ComponentTh;
import fisher.runtime.Frameable;
import fisher.runtime.Thing;
import fisher.runtime.security.Security;
import fisher.statics.Seal;
import fisher.syn.Case;
import fisher.syn.Cmd;
import fisher.syn.FunDecl;
import fisher.syn.ImportStmt;
import fisher.syn.ProcBody;
import fisher.syn.ProcInit;
import fisher.syn.Recv;
import fisher.syn.Spawn;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.syn.interfaces.ComponentInfo;
import fisher.syn.interfaces.LocalMember;
import fisher.syn.interfaces.ProcMember;
import fisher.test.TestUtils;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;
import fisher.util.SpecialCharacters;

public  class  ComponentThread extends Thread  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public Frame baseFrame;
	public final ComponentInfo component;
	public final ComponentTh compTh;
	public final SiteData siteData;
	public final Mailbox mailbox = new Mailbox(this);
	public static long counter = 0;
	public final long number = ComponentThread.counter++;
	public boolean initted = false;
	public boolean finished = false;
	public Framelike tempSpawnerFrame;
	public Letter lastLetter = null;
	public Evaller evaller; 

	private ComponentThread(ComponentInfo component, SiteData siteData, Framelike spawnerFrame) {
		super();
		this.component = component;
		this.siteData = siteData;
		this.compTh = siteData.newCompTh(component);
		this.tempSpawnerFrame = spawnerFrame;
		this.setName(this.toString());
	}

	public static ComponentThread mine() {
		return (ComponentThread) Thread.currentThread();
	}

	public static ComponentThread start(SiteData siteData, ComponentInfo src, Framelike frame) throws FisherException {
		ComponentThread ct = new ComponentThread(src, siteData, frame);
		siteData.noteNewComponent(ct);
		ct.start();
		return ct;
	}

	@Override
	public String toString() {
		return SpecialCharacters.COMPONENT + compTh;
	}
	
	private void snagGlobalsFromSpawnerFrame() throws FisherException{
		Syntax src = (Syntax) component;
		final Set<Seal> freeVarSeals = component.freeVarSeals();
//		System.err.println("ComponentThread: freeVarSeals = " + Bard.sep(freeVarSeals, ",") + "\ncomponent=" + component);
		for (Seal fv : freeVarSeals ){
			Frameable f = tempSpawnerFrame.baseValue(fv, src);
			if (f.transmissible() || fv.builtIn) {
				baseFrame.store(fv, f, src);
			}
			else {
				Doom.runtime("Attempt to give a non-transmissible value to a new component", src, "\nvalue = " + f, "\nIt is of type: " + f.getClass(), this);
			}
		}
	}

	public void run() {
		try {
			this.evaller = Evaller.of((Cmd) component);
			baseFrame = evaller.rootFrame;
			
			for (ProcMember pm : component.bits()) {
				if (pm instanceof ImportStmt) {
					ImportStmt imps = (ImportStmt) pm;
					evaller.eval(imps, baseFrame);
				}
			}
			
			snagGlobalsFromSpawnerFrame();
			
			
			SiteData.register(siteData);
			
			if (TestUtils.printComponentThread) {
				System.err.println("ComponentThread.run: started " + this);
			}
			
			
			
			for (FunDecl fd : component.funs()) {
				evaller.eval(fd, baseFrame);
			}
			for (LocalMember c : component.localMembers()) {
				evaller.eval((Cmd) c, baseFrame);
				if (TestUtils.printComponentThread) {
					System.err.println("ComponentThread.run(" + this + ") : did " + c);
				}
			}
			ProcInit init = component.init();
			if (init != null) {
				evaller.eval(init.initcode, baseFrame);
			}
			initted = true;
			if (TestUtils.printComponentThread) {
				System.err.println("ComponentThread.run(" + this + "): did init: " + component.init());
			}
			final ProcBody body = component.body();
			evaller.eval(body.bodycode, baseFrame);
			finished = true;
			if (TestUtils.printComponentThread) {
				System.err.println("ComponentThread.run(" + this + "): did body: " + body);
			}

		} catch (Exception e) {
			//e.printStackTrace();
			System.err.println(e.toString());
			TestUtils.noteException(e);
			initted = true;
			finished = true;
		}
	}

	private static class RecvResult {
		public boolean gotIt;
		public Thing retVal;
		public RecvResult(boolean gotIt, Thing retVal) {
			super();
			this.gotIt = gotIt;
			this.retVal = retVal;
		}
		public static final RecvResult no = new RecvResult(false, null);
	}

	public Thing recv(Recv src, Framelike frame) throws FisherException {
		TestUtils.say(TestUtils.printRecv, this, "RECV: " + src);
		long startTime = System.currentTimeMillis();
		long timeout = evalTimeoutLength(src, frame);
		long endTime = startTime + timeout;
		
		int nInitiallyInMailbox = mailbox.queue.size();
		
		RecvResult r1 = findMatchingMessageInCurrentMailbox(src, frame, endTime, nInitiallyInMailbox);
		if (r1.gotIt) {
			return r1.retVal;
		}
		
		RecvResult r2 = dealWithIncomingMessagesUntilTimeout(src, frame, endTime, nInitiallyInMailbox);
		if (r2.gotIt) return r2.retVal;
		Thing valueOfTimeoutClause = evalTimeoutClause(src, frame);
		return valueOfTimeoutClause;
	}

	public static final 		long aThousandYears = 1000L * 60 * 60 * 24 * 356 * 1000;
	private long evalTimeoutLength(Recv src, Framelike frame) throws FisherException {
		Evaller evaller = Evaller.mine();
		if (src.timeoutLen == null ) {
			// Wait a thousand years.
			return aThousandYears; 
		}
		Thing thlength = evaller.eval(src.timeoutLen, frame);
		if (thlength == null) return aThousandYears;
		long length = thlength.asLong(src);
		if (length < 0) length = 0;
		return length;
	}
	
	private RecvResult findMatchingMessageInCurrentMailbox(Recv src, Framelike frame, long endTime, int nInitiallyInMailbox) throws FisherException {
		return scanSegmentOfMailbox(src, frame, 0, nInitiallyInMailbox-1);
	}
	
	
	private RecvResult dealWithIncomingMessagesUntilTimeout(Recv src, Framelike frame, long endTime, int nInitiallyInMailbox) throws FisherException {
		int prevEnd = nInitiallyInMailbox-1;
		long timeToWait;
		do {
			timeToWait = endTime - System.currentTimeMillis();
			if (timeToWait > 1000) timeToWait = 1000; 
			if (timeToWait > 0) { // Remember, wait(0) means wait forever, so
				// don't do that.
				try {
					TestUtils.say(TestUtils.printMailbox, this, "About to wait."); 
					synchronized(mailbox.lock()) {
						mailbox.lock().wait(timeToWait); // released by notifyAll in Mailbox.putLetterInQueue
					}
					TestUtils.say(TestUtils.printMailbox, this, "Finished a wait."); 
				} catch (InterruptedException e) {
					// Interrupted (not done in current impl), 
					// so let's check the new stuff in the mailbox.
				}
			}
			int nCurrentlyInMailbox = mailbox.queue.size();
			RecvResult rr = scanSegmentOfMailbox(src, frame, prevEnd + 1, nCurrentlyInMailbox - 1);
			if (rr.gotIt) return rr;
			prevEnd = nCurrentlyInMailbox - 1;
		} while (timeToWait > 0);
		return RecvResult.no;
	}
	
	private RecvResult scanSegmentOfMailbox(Recv src, Framelike frame, int min, int max) throws FisherException {
		CopyOnWriteArrayList<Letter> queue = mailbox.queue;
		assert(queue.size() > max);
		/* Concurrency notes: 
		 * queue may get stuff added to the end, but that's after position max.
		 * So it's OK (if I understand CopyOnWriteArrayList properly -- 
		 * I've never used it before) to munge stuff in the range min..max.
		 */
		//
		// Look for high-prio messages first.
		List<List<Case>> casesByPrio = src.casesByPrio;
		for(List<Case> casesAtCurrentPrio : casesByPrio) {
			// Check each message in the queue to see if it's one of the messages at the current prio.
			TestUtils.say(TestUtils.printMailbox, this, "Scanning msgs " + min + ".." + max + " for "+ Bard.sep(casesAtCurrentPrio, " | ") ); 
			for(int i = min; i <= max; i++) {
				Letter letter = queue.get(i);
				Thing contents = letter.contents();
				Thing sender = letter.sender();
				TestUtils.say(TestUtils.printMailbox, this, "(" + i + ") contents = " + contents + "; sender = " + sender);
				for(Case acase : casesAtCurrentPrio) {
					TestUtils.say(TestUtils.printMailbox, this, "(" + i + ") vs. " + acase.pat);
					boolean msgMatch = Matchiste.match(acase.pat, contents, frame);
					boolean fromNull = acase.from == null;
					boolean fromMatch = fromNull || Matchiste.match(acase.from, sender, frame);
					boolean envelopeMatch = acase.envelope == null || Matchiste.match(acase.envelope, (Thing)letter, frame);
					TestUtils.say(TestUtils.printMailbox, this, "(" + i + ") msgMatch = " + msgMatch + "; fromNull = " + fromNull + " fromMatch=" + fromMatch);

					if (msgMatch && (fromMatch) && envelopeMatch) {
						// Woot! Found a match!
						// Remove it first, in case the clause throws an exception or does another recieve.
						this.lastLetter = letter;
						queue.remove(i);
						TestUtils.say(TestUtils.printMailbox, this, "FOUND MATCH (" + i + ") contents = " + contents + "; sender = " + sender);
						// Security check
						if (acase.checked) {
							Security.checkSecurity(letter.contents(), letter.securityInfo());
						}
						// Finally, evaluate the clause.
						Thing clauseValue = Evaller.mine().eval(acase.body, frame);
						return new RecvResult(true, clauseValue);
					}
				}// acase
			}// i
		}// casesAtCurrentPrio
		return RecvResult.no;
	}
	
	private Thing evalTimeoutClause(Recv src, Framelike frame)throws FisherException {
		Evaller evaller = Evaller.mine();
		if (src.timeoutCmd == null) return null;
		return evaller.eval(src.timeoutCmd, frame);
	}

}
