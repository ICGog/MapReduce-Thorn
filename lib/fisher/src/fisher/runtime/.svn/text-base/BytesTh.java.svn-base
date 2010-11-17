
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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.syn.core.Syntax;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  BytesTh extends ThingImmutable implements Applyable  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }

	private byte[] bytes;

	public BytesTh(byte[] bytes) {
		this.bytes = bytes;
	}
	
	public boolean isBytes() {return true;}
	
	public byte[] asBytes(Syntax src) {return bytes;}

	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		return false;
	}

	@Override
	public String typeString() {
		return "bytes";
	}
	
	@Override
	public boolean transmissible() {
		return true;
	}
	
	@Override
	public Object unthingify() {
		return this.bytes;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj == null || obj.getClass() != BytesTh.class) return false;
		BytesTh b = (BytesTh) obj;
		if (bytes.length != b.bytes.length) return false;
		for (int i = 0; i < bytes.length; i++) {
			if (bytes[i]!= b.bytes[i]) return false;
		}
		return true;
	}
	
	public Thing apply(Thing[] args, Framelike frame, Evaller evaller, Syntax src) throws FisherException {
		checkNumberOfArgs(1, 1, "()", args, evaller, frame, src);
		int i = args[0].asJavaInt(src);
		if (i < 0 || i >= bytes.length) {
			Doom.runtime("Index out of bounds", src, "\nindex = " + i, "\nbytes length = " + bytes.length);
		}
		byte bi = bytes[i];
		return IntTh.of(bi);
	}

	@Override
	public int hashCode() {
		int hc = 9;
		for (byte b : this.bytes) {
			hc = (hc * 2) + b;
		}
		return hc;
	}
	
	private final static int STR = 0;
	private final static int NUM = 1;
	private final static int Clst = 2;

	private static Map<String, Integer> methodCode = new HashMap<String, Integer>();
	static {
		methodCode.put("num", NUM);
		methodCode.put("len", NUM);
		methodCode.put("str", STR);
		methodCode.put("lst", Clst);
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
				return this.NUM(methodName, args, evaller, frame, src);
			case Clst:
				return this.Clst(methodName, args, evaller, frame, src);

			default:
				Doom.internal("Method key structure broken", src, methodC);
				return null;
			}
		}
		return this.tryThingMethods(methodName, args,  src);
	}
	
	private ListTh Clst(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		List<Thing> b = new ArrayList<Thing>(this.bytes.length);
		for (int i = 0; i < this.bytes.length; i++) {
			Thing ti = IntTh.of(this.bytes[i]);
			b.add(ti);
		}
		ListTh L = ListTh.fromJavaList(b);
		return L;
	}
	private IntTh NUM(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return IntTh.of(this.bytes.length);
	}
	
	@Override
	public String toString() {
		return new String(bytes);
	}
	
	
}
