
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

import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  BoolTh extends ThingImmutable  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public final boolean val;
	
	private BoolTh(boolean val) {this.val = val;}
	
	public static final BoolTh True = new BoolTh(true);
	public static final BoolTh False = new BoolTh(false);
	
	public static BoolTh of(boolean b) {
		return b ? True : False;
	}
	
	public String toString() {
		return ""+val;
	}
	
	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		return true;
	}
	
	
	@Override
	public Object unthingify() {
		return this.val;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj == null)return false;
		else if (obj instanceof BoolTh) {
			BoolTh b = (BoolTh) obj;
			return this.val == b.val;
		}
		else return false;
	}
	
	@Override
	public int hashCode() {
		return val ? 1 : 0;
	}
	
	@Override
	public boolean asBoolean(Syntax src) throws FisherException {
		return this.val;
	}
	@Override
	public boolean isBoolean() {
		// TODO Auto-generated method stub
		return true;
	}
	@Override
	public String typeString() {
		return "bool";
	}

//
//	public Internal_Success invokePat(Id patId, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
//	throws FisherException {
//		return this.patNotUnderstood(patId, args, evaller, frame, src);
//	}
	

	private final static int STR = 0;
	private final static int NUM = 1;
	private final static int ChashCode = 2;
	private static Map<String, Integer> methodCode = new HashMap<String, Integer>();
	static {
		methodCode.put("str", STR);
		methodCode.put("num", NUM);
		methodCode.put("hashCode", ChashCode);
	}

	@Override
	public Thing invokeMethod(String methodName, Thing[] args, Syntax src)
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
			case ChashCode : return  this.ChashCode(methodName, args, evaller, frame, src); 
			default:
				Doom.internal("Method key structure broken", src, methodC);
				return null;
			}
		}
		return this.tryThingMethods(methodName, args,  src);
	}
	
	
}
