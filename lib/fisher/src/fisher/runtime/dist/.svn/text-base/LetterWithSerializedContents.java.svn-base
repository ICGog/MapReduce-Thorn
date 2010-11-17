
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

import fisher.runtime.Thing;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.util.Bard;

public abstract  class  LetterWithSerializedContents extends AbstractLetter implements Letter  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	private final byte[] serializedContents;
	private final byte[] serializedSecurityInfo;
	
	private Thing contents ;
	private boolean contentsValid = false; 
	

	
	protected LetterWithSerializedContents(Thing contents, Thing securityInfo) {
		this.serializedContents = Bard.serialization(contents);
		this.serializedSecurityInfo = Bard.serialization(securityInfo);
	}
	
	public Thing contents() {
		if (!contentsValid) {
			contents = (Thing) Bard.deserialization(serializedContents);
			contentsValid = true;
		}
		return contents;
	}
	
	protected Thing  securityInfo;
	private boolean securityInfoValid = false;
	
	public Thing securityInfo() {
		if (!securityInfoValid) {
			securityInfo = (Thing) Bard.deserialization(serializedSecurityInfo);
			securityInfoValid = true;
		}
		return securityInfo;
	}
	
	/*
	 This class is intended to deal with a stupid and annoying problem concerned with 
	 transmitting Thorn objects.
	 When a Thorn object is received, its Thorn class is resolved 
	  >>> in the context of the receiving component <<<
	 I'm using Java deserialization to resolve the class -- 
	 that's the methods ClassDynamic.readObject and readResolve.
	 
	 Suppose we've got class C(x){}, and we're sending C(1) 
	 from P to Q.
	 
	 This is using the standard Java readResolve() mechanism, 
	 with which, when one object is deserialized, it can be replaced
	 by another, extant object.  Which is what is wanted here: 
	 When Q gets the string "serC(1)" -- or whatever the serialization of 
	 C(1) looks like, which is not nearly that pretty -- it will deserialize
	 it. As part of deserialization, it'll start deserializing the string 
	 representing C(1).  We don't want to send a copy of the ClassDynamicTh 
	 of class C on P to Q, though; we want to use Q's copy.  The evaller
	 keeps a hash table of classes, indexed by their serializationKey().
	 So, when P serializes C(1), it serializes the classDynamic field as just 
	 a little container for the serializationKey().  When Q deserializes it, 
	 following the usual Java protocol, it first produces a crap 
	 ClassDynamicTh which (by the code in ClassDynamicTh.readObject()) 
	 just has the key. Then Java deserialization calls readResolve(), which 
	 looks up the actual ClassDynamicTh corresponding to that key --
	 in the hash table of the current thread's Evaller.
	 
	 That last clause introduces a whole dimension of pain. 
	 We can only deserialize it from the thread that is supposed to get it.
	 Other threads either have the wrong set of Thorn classes available, or aren't even
	 ComponentThreads and thus have *no* evaller and hence no available Thorn classes.
	 
	 The solution here is to keep the contents serialized until they get to the right 
	 thread, and unserialize them there.
	 
	 But that means you have to be really really careful when other threads have their
	 claws on a LetterWithSerializedContents.  The first time you call contents(), 
	 you deserialize the contents, and that *has* to be done in the right thread, or it'll
	 screw up internal data structures in a tricky complicated way.
	 */

}
