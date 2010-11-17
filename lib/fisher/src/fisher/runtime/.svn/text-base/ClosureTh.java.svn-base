
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.runtime;

import java.util.HashMap;
import java.util.Map;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.Frame;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.statics.purity.PurityStatus;
import fisher.statics.purity.StaticPurityChecker;
import fisher.syn.FunBody;
import fisher.syn.MonoBody;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  ClosureTh extends ThingBuiltIn implements Applyable  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	@Override
	public String typeString() {
		return "fn";
	}

	public boolean isClosure() {
		return true;
	}

	public Framelike surroundingFrame;
	public FunBody funBody;
	public final boolean pure;

	
	
	public ClosureTh(Framelike surroundingFrame, FunBody funBody) {
		super();
		this.surroundingFrame = surroundingFrame;
		this.funBody = funBody;
		try {
			StaticPurityChecker.check(funBody); // cached, really.
		} catch (Exception e) {
			Doom.internalCatastrophe("Trouble deep in purity checking", funBody);
		}
		this.pure = funBody.purityStatus() == PurityStatus.PURE;
	}
	
	@Override
	public boolean transmissible() {
		return false;
	}
	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		return this.pure;
	}

	public Thing apply(Thing[] args, Framelike ignoredFrame, Evaller evaller, Syntax src) throws FisherException {
		Frame applyFrame = Frame.inner(surroundingFrame);
		for (MonoBody monobody : this.funBody.funbodies) {
			// NOTE: disjoint seals used here.
			if (evaller.evalFormals(monobody.formals, args, applyFrame)) {
				Thing res = evaller.eval(monobody.body, applyFrame);
				return res;
			}
		}
		return EvalUtil.applyFailed(this, this.funBody, args, ignoredFrame, evaller);
	}

	public String toString() {
		return "fn " + funBody.toString();
	}

	private final static int STR = 0;
	private final static int NUM = 1;
	private static Map<String, Integer> methodCode = new HashMap<String, Integer>();
	static {
		methodCode.put("str", STR);
		methodCode.put("num", NUM);
	}

	@Override
	public Thing invokeMethod(String methodName, Thing[] args,   Syntax src)
			throws FisherException {
		if (methodCode.containsKey(methodName)) {
			Integer methodC = methodCode.get(methodName);
			Evaller evaller = Evaller.mine();
			Framelike frame = this.frameForInvoke();
			switch (methodC) {
			case STR:
				return this.str();
			case NUM:
				return this.tryThingMethods(methodName, args,   src);
			default:
				Doom.internal("Method key structure broken", src, methodC);
				return null;
			}
		}
		return this.tryThingMethods(methodName, args,    src);
	}


//	public Internal_Success invokePat(Id patId, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
//	throws FisherException {
//		return this.patNotUnderstood(patId, args, evaller, frame, src);
//	}
}
