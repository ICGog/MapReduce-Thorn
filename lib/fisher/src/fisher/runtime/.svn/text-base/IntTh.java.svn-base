
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
import java.util.Random;

import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  IntTh extends ThingImmutable  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	long value;

	static public IntTh[] posCache = new IntTh[] { new IntTh(0), new IntTh(1), new IntTh(2), new IntTh(3),
			new IntTh(4), new IntTh(5), new IntTh(6), };

	public static IntTh of(long value) {
		if (0 <= value && value < posCache.length)
			return posCache[(int) value];
		return new IntTh(value);
	}

	private IntTh(long value) {
		super();
		this.value = value;
	}

	@Override
	public boolean transmissible() {
		return true;
	}

	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		return true;
	}
	
	@Override
	public Object unthingify() {
		return this.value;
	}

	@Override
	public boolean equals(Object obj) {
		return obj != null && obj.getClass() == IntTh.class && ((IntTh) obj).value == this.value;
	}

	@Override
	public int hashCode() {
		return (int) (value % (1024 * 256));
	}

	public String toString() {
		return "" + value;
	}

	@Override
	public boolean isLong() {
		// TODO Auto-generated method stub
		return true;
	}

	public boolean isNumber() {
		return true;
	}

	public double asDouble(Syntax src) {
		return (double) value;
	}

	@Override
	public long asLong(Syntax syn) throws FisherException {
		return value;
	}

	@Override
	public String typeString() {
		return "int";
	}

	private final static int STR = 0;
	private final static int PRIMEP = 2;
	private final static int NUM = 1;
	private final static int CbitAnd = 3;
	private final static int CbitOr = 4;
	private final static int CbitXor = 5;
	private final static int CbitNot = 6;
	private final static int CbitShl = 7;
	private final static int CbitShr = 9;
	private final static int CbitShr0 = 10;
	private final static int CstrBin = 11;
	private final static int Cbits = 12;
	private final static int CbitSet = 13;
	private final static int CstrHex = 14;
	private final static int CstrOct = 15;
	private final static int CstrBase = 16;
	private final static int Csgn = 17;
	private final static int Cabs = 18;
	private final static int Cpow = 19;
	private final static int Cmax = 20;
	private final static int Cmin = 21;
	private final static int Ctrim = 22;
	private final static int Crand0 = 23;
	private final static int Crand1 = 24;
	private final static int ChashCode = 25;
	private final static int Cplus = 26;
	private final static int Cminus = 27;
	private final static int Cdiv = 28;
	private final static int just_return_this = 29;
	private final static int Cmod = 30;
	private final static int Cchar = 31;
	private final static int Cdotdot = 32;

	private static Map<String, Integer> methodCode = new HashMap<String, Integer>();
	static {
		methodCode.put("prime?", PRIMEP);
		methodCode.put("num", NUM);
		methodCode.put("str", STR);
		methodCode.put("bitAnd", CbitAnd);
		methodCode.put("bitOr", CbitOr);
		methodCode.put("bitXor", CbitXor);
		methodCode.put("bitNot", CbitNot);
		methodCode.put("bitShl", CbitShl);
		methodCode.put("bitShr", CbitShr);
		methodCode.put("bitShr0", CbitShr0);
		methodCode.put("strBin", CstrBin);
		methodCode.put("bits", Cbits);
		methodCode.put("bitSet", CbitSet);
		methodCode.put("strHex", CstrHex);
		methodCode.put("strOct", CstrOct);
		methodCode.put("strBase", CstrBase);
		methodCode.put("sgn", Csgn);
		methodCode.put("abs", Cabs);
		methodCode.put("pow", Cpow);
		methodCode.put("max", Cmax);
		methodCode.put("min", Cmin);
		methodCode.put("trim", Ctrim);
		methodCode.put("rand0", Crand0);
		methodCode.put("rand1", Crand1);
		methodCode.put("hashCode", ChashCode);
		methodCode.put("+", Cplus);
		methodCode.put("-", Cminus);
		methodCode.put("div", Cdiv);
		methodCode.put("round", just_return_this);
		methodCode.put("floor", just_return_this);
		methodCode.put("ceil", just_return_this);
		methodCode.put("mod", Cmod);
		methodCode.put("char", Cchar);
		methodCode.put("..", Cdotdot);
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
			case PRIMEP:
				return this.Mprimep();
			case NUM:
				return this.Mnum();
			case CbitAnd:
				return this.MbitAnd(methodName, args, evaller, frame, src);
			case CbitOr:
				return this.MbitOr(methodName, args, evaller, frame, src);
			case CbitXor:
				return this.MbitXor(methodName, args, evaller, frame, src);
			case CbitNot:
				return this.MbitNot(methodName, args, evaller, frame, src);
			case CbitShl:
				return this.MbitShl(methodName, args, evaller, frame, src);
			case CbitShr:
				return this.MbitShr(methodName, args, evaller, frame, src);
			case CbitShr0:
				return this.MbitShr0(methodName, args, evaller, frame, src);
			case CstrBin:
				return this.MstrBin(methodName, args, evaller, frame, src);
			case Cbits:
				return this.Cbits(methodName, args, evaller, frame, src);
			case CbitSet:
				return this.CbitSet(methodName, args, evaller, frame, src);
			case CstrHex:
				return this.CstrHex(methodName, args, evaller, frame, src);
			case CstrOct:
				return this.CstrOct(methodName, args, evaller, frame, src);
			case CstrBase:
				return this.CstrBase(methodName, args, evaller, frame, src);
			case Csgn:
				return this.Csgn(methodName, args, evaller, frame, src);
			case Cabs:
				return this.Cabs(methodName, args, evaller, frame, src);
			case Cpow:
				return this.Cpow(methodName, args, evaller, frame, src);
			case Cmax:
				return this.Cmax(methodName, args, evaller, frame, src);
			case Cmin:
				return this.Cmin(methodName, args, evaller, frame, src);
			case Ctrim:
				return this.Ctrim(methodName, args, evaller, frame, src);
			case Crand0:
				return this.Crand0(0, methodName, args, evaller, frame, src);
			case Crand1:
				return this.Crand0(1, methodName, args, evaller, frame, src);
			case ChashCode:
				return this.ChashCode(methodName, args, evaller, frame, src);
			case Cplus:
				return this.Cplus(methodName, args, evaller, frame, src);
			case Cminus:
				return this.Cminus(methodName, args, evaller, frame, src);
			case Cdiv:
				return this.Cdiv(methodName, args, evaller, frame, src);
			case just_return_this:
				return this;
			case Cmod:
				return this.Cmod(methodName, args, evaller, frame, src);
			case Cchar:
				return this.Cchar(methodName, args, evaller, frame, src);
			case Cdotdot:
				return this.Cdotdot(methodName, args, evaller, frame, src);
			default:
				Doom.internal("Method key structure broken", src, methodC);
				return null;
			}
		}
		return this.tryThingMethods(methodName, args,    src);
	}

	protected IntTh ChashCode(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		return IntTh.of((int) value);
	}

	private Thing Cdotdot(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		long high = args[0].asLong(src);
		return  IntRangeTh.of(value, high);
	}
	private CharTh Cchar(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		if (value >= 0 && value <= Integer.MAX_VALUE) {
			int i = (int) value;
			CharTh c = CharTh.of(i, src);
			return c;
		} else {
			Doom.runtime("Not even close to a Unicode code point: " + value, src);
			return null;
		}
	}

	private IntTh Crand0(int i, String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		if (value < 0)
			Doom.runtime("Can't roll a negative-sided die.", src, value);
		if (value == 0)
			return IntTh.of(0);
		long r;
		if (value < Integer.MAX_VALUE)
			r = Math.abs(Bard.random.nextInt((int) value));
		else
			r = Math.abs(Bard.random.nextLong()) % value;
		return IntTh.of(r + i);
	}

	private IntTh Ctrim(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		long low = args[0].asLong(src);
		long high = args[1].asLong(src);
		return IntTh.of(Math.min(high, Math.max(low, value)));
	}

	private Thing Cminus(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		Thing m = args[0];
		if (m instanceof IntTh) {
			IntTh im = (IntTh) m;
			return IntTh.of(this.value - im.value);
		} else if (m instanceof FloatTh) {
			FloatTh fth = (FloatTh) m;
			return FloatTh.of(this.value - fth.value);
		}

		else {
			Doom.runtime("Can't subtract " + this + "-" + m, src, m);
			return null;
		}
	}

	private Thing Cdiv(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		Thing m = args[0];
		if (m instanceof IntTh) {
			IntTh im = (IntTh) m;
			return IntTh.of(this.value / im.value);
		} else if (m instanceof FloatTh) {
			FloatTh fth = (FloatTh) m;
			return FloatTh.of((long) (this.value / fth.value));
		}

		else {
			Doom.runtime("Can't divide " + this + " idiv " + m, src, m);
			return null;
		}
	}

	private Thing Cmod(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		Thing m = args[0];
		if (m instanceof IntTh) {
			IntTh im = (IntTh) m;
			return IntTh.of(this.value % im.value);
		} else {
			Doom.runtime("Can't compute " + this + " mod " + m, src, m);
			return null;
		}
	}

	private Thing Cplus(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		Thing m = args[0];
		if (m instanceof IntTh) {
			IntTh im = (IntTh) m;
			return IntTh.of(im.value + this.value);
		} else if (m instanceof FloatTh) {
			FloatTh fth = (FloatTh) m;
			return FloatTh.of(fth.value + this.value);
		} else if (m instanceof StringTh) {
			StringTh sth = (StringTh) m;
			return StringTh.of(this.value + sth.value);
		} else {
			Doom.runtime("Can't add " + this + "+" + m, src, m);
			return null;
		}
	}

	

	private IntTh Cmax(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		if (args[0].isLong()) {
			long m = args[0].asLong(src);
			return IntTh.of(Math.max(m, value));
		}
		else if (args[0] instanceof Iterable) {
			long m = this.value;
			for(Thing th : args[0].asIter(src)) {
				m = Math.max(m, th.asLong(src));
			}
			return IntTh.of(m);
		}
		else {
			Doom.runtime("Can't take max of " + args[0], src);
			return null;
		}
	}
	private IntTh Cmin(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		if (args[0].isLong()) {
			long m = args[0].asLong(src);
			return IntTh.of(Math.min(m, value));
		}
		else if (args[0] instanceof Iterable) {
			long m = this.value;
			for(Thing th : args[0].asIter(src)) {
				m = Math.min(m, th.asLong(src));
			}
			return IntTh.of(m);
		}
		else {
			Doom.runtime("Can't take min of " + args[0], src);
			return null;
		}
	}

	private IntTh Cbits(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		long i = args[0].asLong(src);
		long j = args[1].asLong(src);
		if (i > j)
			return IntTh.of(0);
		int n = 63;
		String sv = Long.toBinaryString(value);
		//		System.out.println("v = " + sv);
		long a = value << (n - j);
		String sa = Long.toBinaryString(a);
		//		System.out.println("a = " + sa);
		long b = a >>> (i + n - j);
		String sb = Long.toBinaryString(b);
		//		System.out.println("b = " + sb);
		return IntTh.of(b);
	}

	private IntTh CbitSet(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(3, 3, methodName, args, evaller, frame, src);
		long i = args[0].asLong(src);
		long j = args[1].asLong(src);
		long v = args[2].asLong(src);
		if (i > j)
			return IntTh.of(0);
		int n = 63;
		//		System.out.println("v = " + Long.toBinaryString(value));
		long l = (value >>> (j + 1)) << (j + 1);
		//		System.out.println("l=" + Long.toBinaryString(l));
		long r = (value << (n - i + 1)) >>> (n - i + 1);
		//		System.out.println("r=" + Long.toBinaryString(r));
		long V = (v << (n - (j - i))) >>> (n - (j - i)); // v, trimmed to the right bits
		//		System.out.println("V=" + Long.toBinaryString(V));
		long m = V << i;
		//		System.out.println("m=" + Long.toBinaryString(m));
		long b = r | m | l;
		//		System.out.println("b=" + Long.toBinaryString(b));
		return IntTh.of(b);

	}

	private StringTh CstrHex(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return StringTh.of(Long.toHexString(value));
	}

	private StringTh CstrOct(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return StringTh.of(Long.toOctalString(value));
	}

	private StringTh CstrBase(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		long n = args[0].asLong(src);
		return StringTh.of(Long.toString(value, (int) n));
	}

	private IntTh Cpow(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		long n = args[0].asLong(src);
		return IntTh.of(Bard.longPower(value, n));
	}

	private IntTh MbitAnd(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		long n = args[0].asLong(src);
		return IntTh.of(n & value);
	}

	private IntTh MbitOr(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		long n = args[0].asLong(src);
		return IntTh.of(n | value);
	}

	private IntTh MbitXor(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		long n = args[0].asLong(src);
		return IntTh.of(n ^ value);
	}

	private IntTh MbitShl(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		long n = args[0].asLong(src);
		return IntTh.of(value << n);
	}

	private IntTh MbitShr(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		long n = args[0].asLong(src);
		return IntTh.of(value >> n);
	}

	private IntTh MbitShr0(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		long n = args[0].asLong(src);
		return IntTh.of(value >>> n);
	}

	private IntTh MbitNot(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return IntTh.of(~value);
	}

	private IntTh Cabs(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return IntTh.of(value < 0 ? -value : value);
	}

	private IntTh Csgn(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return IntTh.of(Long.signum(value));
	}

	private StringTh MstrBin(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return StringTh.of(Long.toBinaryString(value));
	}

	private BoolTh Mprimep() {
		if (this.value <= 1)
			return BoolTh.False;
		if (this.value <= 3)
			return BoolTh.True;
		for (long i = 2; i * i <= value; i++) {
			if (value % i == 0)
				return BoolTh.False;
		}
		return BoolTh.True;
	}

	private IntTh Mnum() {
		return this;
	}
	//
	//	public Internal_Success invokePat(Id patId, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	//			throws FisherException {
	//		return this.patNotUnderstood(patId, args, evaller, frame, src);
	//	}

}
