
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
import java.util.Iterator;
import java.util.Map;

import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;
import fisher.util.FisherRuntimeException;

public  class  IntRangeTh extends AbstractRangeTh implements Iterable<Thing>, Applyable { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public final long min;
	public final long max; 
	public final long length;
	private IntRangeTh(long min, long max)  {
		super();
		if (min > max) {
			min = 0;
			max = -1;
		}
		this.min = min;
		this.max = max;
		this.length = max - min + 1;
	}
	
	
	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		return true;
	}
	
	public static IntRangeTh EMPTY = new IntRangeTh(0,-1);
	
	public static IntRangeTh of(long min, long max) {
		if (min > max) return EMPTY;
		else return new IntRangeTh(min, max);
	}
	
	@Override
	public boolean canBeKey() {
		return true;	
	}
	
	public String toString() {
		return min + ".." + max;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof IntRangeTh) {
			IntRangeTh other = (IntRangeTh) obj;
			return other.max == this.max && this.min == other.min;
		}
		else return false;
	}
	
	@Override
	public boolean isIntRange() {
		return true;
	}
	
	@Override
	public IntRangeTh asIntRange(Syntax src) throws FisherException {
		return this;
	}
	
	@Override
	public int hashCode() {
		return (int)(min ^ max);
	}
	
	@Override
	public Thing low() {
		return IntTh.of(min);
	}
	
	@Override
	public Thing high() {
		return IntTh.of(max);
	}
	
	@Override
	public Iterable<Thing> asIter(Syntax src) throws FisherException {
		return this;
	}
	
	public Iterator<Thing> iterator() {
		return new Iterator<Thing>() {
			private long i = min;
			public Thing next() {
				return  IntTh.of(i++);
			}
			public boolean hasNext() {
				return i <= max;
			}
			public void remove() {
				throw new UnsupportedOperationException();
			}
		};
	}
	
	
	public Thing apply(Thing[] args, Framelike frame, Evaller evaller, Syntax src) throws FisherException {
		checkNumberOfArgs(1, 1, "()", args, evaller, frame, src);
		long i = args[0].asLong(src);
		if (min+i > max || i < 0) {
			Doom.runtime("Subscript out of range: " + this + "(" + i + ")" , src);
		}
		return IntTh.of(min+i);
	}
	
	private final static int STR = 0;
	private final static int NUM = 1;
	private final static int EMPTYP = 2;
	private final static int CrevIn = 3;
	private final static int ChashCode = 4;
	private final static int Ctrim = 5;
	private final static int Cmin = 6;
	private final static int Cmax = 7;
	private final static int Crand = 8;
	
	private static Map<String, Integer> methodCode = new HashMap<String, Integer>();
	static {
		methodCode.put("str", STR);
		methodCode.put("len", NUM);
		methodCode.put("num", NUM);
		methodCode.put("empty?", EMPTYP);
		methodCode.put("revIn", CrevIn);
		methodCode.put("hashCode", ChashCode);
		methodCode.put("trim", Ctrim);
		methodCode.put("min", Cmin);
		methodCode.put("max", Cmax);
		methodCode.put("rand", Crand);
	}

	@Override
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
				return this.Mnum();
			case EMPTYP: 
				return this.Memptyp();
			case CrevIn:
				return this.CrevIn(methodName, args, evaller, frame, src);
			case ChashCode:
				return this.ChashCode(methodName, args, evaller, frame, src);
			case Ctrim:
				return this.Ctrim(methodName, args, evaller, frame, src);
			case Cmin:
				return this.Cmin(methodName, args, evaller, frame, src);
			case Cmax:
				return this.Cmax(methodName, args, evaller, frame, src);
			case Crand:
				return this.Crand(methodName, args, evaller, frame, src);
			default:
				Doom.internal("Method key structure broken", src, methodC);
				return null;
			}
		}
		return this.tryThingMethods(methodName, args, src);
	}
	
	private BoolTh CrevIn(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		long m = args[0].asLong(src);
		boolean b = min <= m && m <= max;
		return BoolTh.of(b);
	}
	
	private IntTh Crand(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		if (max < min) return null;
		final long nextLong = Math.abs(Bard.random.nextLong());
		final long variant = (nextLong % length);
		long r = min + variant;
		return IntTh.of(r);
	}
	private IntTh Cmin(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return IntTh.of(min);
	}
	private IntTh Cmax(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return IntTh.of(max);
	}
	private IntTh Ctrim(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		long m = args[0].asLong(src);
		if (m < min) return IntTh.of(min);
		else if (m > max) return IntTh.of(max);
		else return IntTh.of(m);
	}

	
	private IntTh Mnum() {
		return IntTh.of(length);
	}
	
	private BoolTh Memptyp() {
		return BoolTh.of(length == 0);
	}
	
}

