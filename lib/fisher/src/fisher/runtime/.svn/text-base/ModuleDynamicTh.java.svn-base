
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
import java.util.List;
import java.util.Map;

import fisher.eval.Evaller;
import fisher.eval.Frame;
import fisher.eval.interfaces.Fieldiferous;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.statics.ModuleStatic;
import fisher.statics.Seal;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.test.TestUtils;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  ModuleDynamicTh extends ThingBuiltIn implements Framelike, Fieldiferous  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	public final ModuleStatic moduleStatic;
	public final Framelike frame;
	
	// Framelike works with seals, 
	// but for some uses, we only have field names, not seals.
	// So, membersByName lets us work by field name.
	private Map<String, Seal> membersByName = new HashMap<String, Seal>();
	
	
	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		// TODO: this is too conservative.
		return false;
	}
	
	public ModuleDynamicTh(ModuleStatic moduleStatic, Framelike frame) {
		super();
		this.moduleStatic = moduleStatic;
		this.frame = frame;
		if (TestUtils.TEST_ModuleInstanceTh) {
			TestUtils.noteModuleInstance(this);
		}
	}

	@Override
	public String typeString() {
		return "ModuleInstance";
	}
	
	
	public Thing RValue(Seal seal, Syntax src) throws FisherException {
		return frame.RValue(seal, src);
	}
	
	public VarCell LValue(Seal seal, Syntax src) throws FisherException {
		return frame.LValue(seal, src);
	}
	
	public void store(Seal seal, Frameable f, Syntax src) throws FisherException {
		frame.store(seal, f, src);
		membersByName.put(seal.str(), seal);
	}
	
	public Frameable baseValue(Seal seal, Syntax src) throws FisherException {
//		System.err.println("ModuleDynamicOw: seal=" + seal + " src=" + src);
		return frame.baseValue(seal, src);
	}
	
	public Frameable baseValueByName(String fieldName, Syntax src) throws FisherException {
		if (!this.hasField(fieldName)) {
			Doom.runtime("Module "  + this + " has no field named " + fieldName, src);
		}
		Seal seal = membersByName.get(fieldName);
		return frame.baseValue(seal, src);
	}
	
	public Thing fetchImmediateMemberR(Seal seal, Syntax src) throws FisherException {
		return frame.fetchImmediateMemberR(seal, src);
	}
	
	public boolean hasField(String fieldName) {
		return membersByName.containsKey(fieldName);
	}
	
	public Thing getField(String fieldName, Syntax src) throws FisherException {
		if (!this.hasField(fieldName)) {
			Doom.runtime("Module "  + this + " has no field named " + fieldName, src);
		}
		Seal seal = membersByName.get(fieldName);
		return frame.RValue(seal, src);
	}
	
	public VarCell LValueByName(String fieldName, Syntax src) throws FisherException {
		if (!this.hasField(fieldName)) {
			Doom.runtime("Module "  + this + " has no field named " + fieldName, src);
		}
		Seal seal = membersByName.get(fieldName);
		return frame.LValue(seal, src);
	}
	
	public List<Seal> seals() {
		return frame.seals();
	}
	
	public boolean hasSeal(Seal seal) {
		return frame.hasSeal(seal);
	}
	
	
	public ObjectTh theThis(Evaller evaller, Syntax src) throws FisherException {
		return Doom.noThis(evaller, src);
	}
	

	private final static int STR = 0;
	private final static int NUM = 1;
	private static Map<String, Integer> methodCode = new HashMap<String, Integer>();
	static {
		methodCode.put("str", STR);
		methodCode.put("num", NUM);
	}

	@Override
	// WARNING: This does not utterly follow the pattern! 
	public Thing invokeMethod(String methodName, Thing[] args,  Syntax src)
			throws FisherException {
		if (methodCode.containsKey(methodName)) {
			Integer methodC = methodCode.get(methodName);
			Evaller evaller = Evaller.mine();
			Framelike frame = this.frameForInvoke();
			switch (methodC) {
			case STR:
				return this.str();
			case NUM: 
				return this.tryThingMethods(methodName, args,  src);
			default:
				Doom.internal("Method key structure broken", src, methodC);
				return null;
			}
		}
		// THIS BIT IS NONSTANDARD!
		return callAFunctionInTheModuleIfThereIsOne(methodName, args,   src);
		// return this.methodNotUnderstood(methodName, args, evaller, frame, src);
	}
	
	private Thing callAFunctionInTheModuleIfThereIsOne(String funName, Thing[] args,   Syntax src)
	throws FisherException {
		Thing field = this.getField(funName, src);
		if (field instanceof Applyable) {
			Applyable applyable = (Applyable) field;
			final Thing applied = applyable.apply(args, frame, Evaller.mine(), src);
			return applied;
		}
		else {
			Doom.internal("Not applyable: " + field, src, "args = " + Bard.sep(args, ", "));
			return null;
		}
		
	}
	
	public String toString() {
		return this.moduleStatic.name();
	}
	
	private Thing it = null;
	
	public void setIt(Thing subject, Syntax src) throws FisherException {
		it = subject;
	}
	
	public Thing getIt(Syntax src) throws FisherException {
		return it;
	}

//	public Internal_Success invokePat(Id patId, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
//	throws FisherException {
//		return this.patNotUnderstood(patId, args, evaller, frame, src);
//	}
	
}
