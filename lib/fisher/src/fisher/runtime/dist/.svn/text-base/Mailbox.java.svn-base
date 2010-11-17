
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

import java.util.concurrent.CopyOnWriteArrayList;

import fisher.test.TestUtils;

public  class  Mailbox  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public final ComponentThread componentThread;
	public final CopyOnWriteArrayList<Letter> queue = new CopyOnWriteArrayList<Letter>(); 

	public Mailbox(ComponentThread componentThread) {
		super();
		this.componentThread = componentThread;
	}
	
	public String toString() {
		return "Mailbox(" + componentThread + ")";
	}

	public /* not synchronized */ Object lock() {return queue;}
	
	public /* not synchronized */ void putLetterInQueue(Letter letter) {
		
		
		TestUtils.say(TestUtils.printMailbox, this, this + ": putLetterInQueue(" + letter.toStringWithoutContents() + ")");
		
		queue.add(letter);
		synchronized(this.lock()) {
			// The thread waiting for this lock ought to be a ComponentThread, in dealWithIncomingMessagesUntilTimeout
			// I'm using notifyAll() in case some other thread sometime waits on that lock, but it shouldn't.
			this.lock().notifyAll();
		}
	}
	
}
