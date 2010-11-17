
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
import fisher.eval.interfaces.Framelike;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  FloatTh extends ThingImmutable  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public final double value;

	static public final FloatTh ZERO = new FloatTh(0.0);

	private FloatTh(double d) {
		value = d;
	}

	@Override
	public Object unthingify() {
		return value;
	}
	
	public static FloatTh of(double d) {
		if (d == 0.0)
			return ZERO;
		return new FloatTh(d);
	}

	public String toString() {
		return "" + value;
	}

	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		return true;
	}

	@Override
	public String typeString() {
		// TODO Auto-generated method stub
		return "float";
	}

	@Override
	public boolean equals(Object obj) {
		if (obj.getClass() != FloatTh.class)
			return false;
		return (value == ((FloatTh) obj).value);
	}

	@Override
	public int hashCode() {
		return (int) value;
	}

	@Override
	public boolean isFloat() {
		return true;
	}

	@Override
	public boolean isNumber() {
		return true;
	}

	@Override
	public double asDouble(Syntax src) throws FisherException {
		return value;
	}

	@Override
	public long asLong(Syntax src) throws FisherException {
		return (long) value;
	}

	//	public Internal_Success invokePat(Id patId, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	//			throws FisherException {
	//		// TODO Auto-generated method stub
	//		return this.patNotUnderstood(patId, args, evaller, frame, src);
	//	}

	private final static int STR = 0;
	private final static int NUM = 1;
	private final static int Cwithin = 2;
	private final static int ChashCode = 3;
	private final static int Cplus = 4;
	private final static int Cminus = 5;
	private final static int Cround = 6;
	private final static int Cfloor = 7;
	private final static int Cceil = 8;

	private static Map<String, Integer> methodCode = new HashMap<String, Integer>();
	static {
		methodCode.put("str", STR);
		methodCode.put("num", NUM);
		methodCode.put("within?", Cwithin);
		methodCode.put("hashCode", ChashCode);
		methodCode.put("+", Cplus);
		methodCode.put("-", Cminus);
		methodCode.put("round", Cround);
		methodCode.put("floor", Cfloor);
		methodCode.put("ceil", Cceil);
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
				return this;
			case Cwithin:
				return this.Cwithin(methodName, args, evaller, frame, src);
			case ChashCode:
				return this.ChashCode(methodName, args, evaller, frame, src);
			case Cminus:
				return this.Cminus(methodName, args, evaller, frame, src);
			case Cplus:
				return this.Cplus(methodName, args, evaller, frame, src);
			case Cround:
				return this.Cround(methodName, args, evaller, frame, src);
			case Cfloor:
				return this.Cfloor(methodName, args, evaller, frame, src);
			case Cceil:
				return this.Cceil(methodName, args, evaller, frame, src);
			default:
				Doom.internal("Method key structure broken", src, methodC);
				return null;
			}
		}
		return this.tryThingMethods(methodName, args, src);
	}

	private Thing Cfloor(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return IntTh.of((long) Math.floor(value));		
	}
	
	private Thing Cceil(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return IntTh.of((long)Math.ceil(value));		
	}
	private Thing Cround(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return IntTh.of(Math.round(value));		
	}
	
	
	
	private Thing Cminus(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		if (args[0].isNumber()) {
			double other = args[0].asDouble(src);
			return FloatTh.of(value - other);
		} else {
			Doom.runtime("Can't subtract this from a float: " +args[0], src);
			return null;
		}
		
	}
	private Thing Cplus(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		if (args[0].isNumber()) {
			double other = args[0].asDouble(src);
			return FloatTh.of(other + value);
		} else {
			return StringTh.of(this + EvalUtil.toString(args[0]));
		}

	}

	private BoolTh Cwithin(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		double other = args[0].asDouble(src);
		double tol = args[1].asDouble(src);
		return BoolTh.of(Math.abs(value - other) <= tol);

	}

}
