
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

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.ComponentTh;
import fisher.runtime.Thing;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.syn.core.Syntax;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  MsgComp2Comp extends LetterWithSerializedContents implements Serializable  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public final ComponentTh sender;
	public final ComponentTh receiver;
	

	public MsgComp2Comp(ComponentTh sender, ComponentTh receiver, Thing contents, Thing securityInfo) {
		super(contents, securityInfo);
		this.sender = sender;
		this.receiver = receiver;
	}

	public String toString() {
		return "Msg{" + contents() + " from " + sender + " to " + receiver + "}";
	}
	
	
	public String toStringWithoutContents() {
		return "Msg{ (unopened) " +  " from " + sender + " to " + receiver + "}";
	}

	

	public Thing sender() {
		return sender;
	}

	@Override
	public String typeString() {
		return "msgComp2Comp";
	}

	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		return true;
	}

	private final static int STR = 0;
	private final static int NUM = 1;
	private final static int Csender = 2;

	protected final static int FILE = 100; // DirTh codes are above FILE; FileTh codes are below. 

	private static Map<String, Integer> methodCode = new HashMap<String, Integer>();
	static {
		methodCode.put("str", STR);
		methodCode.put("num", NUM);
		methodCode.put("sender", Csender);
	}

	@Override
	public Thing invokeMethod(String methodName, Thing[] args,  Syntax src)
			throws FisherException {
		if (methodCode.containsKey(methodName)) {
			Integer methodC = methodCode.get(methodName);
			switch (methodC) {
			case STR:
				return this.str();
			case NUM:
				return this.tryThingMethods(methodName, args,  src);
			case Csender:
				return this.Csender(methodName, args,   src);
			default:
				Doom.internal("Method key structure broken", src, methodC);
				return null;
			}
		}
		return super.invokeMethod(methodName, args, src);
	}
	
	private Thing Csender(String methodName, Thing[] args,   Syntax src)
	throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args,   src);
		return this.sender();
	}

}
