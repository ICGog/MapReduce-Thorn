
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
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  Nullity extends ThingImmutable  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public int degree;
	private String str = null;

	public static final List<Nullity> nullities = new ArrayList<Nullity>();
	static {
		nullities.add(null); // position 0;
		makeNullitiesThrough(2);
	}

	public static void makeNullitiesThrough(int n) {
		int lastMade = nullities.size();
		for (int i = lastMade; i <= n; i++) {
			Nullity nullity = new Nullity(i);
			nullities.add(nullity);
		}
		checkClassInvariant();
	}
	
	
	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		return true;
	}

	public static void checkClassInvariant() {
		int i = 0;
		for (Nullity nu : nullities) {
			if (i == 0)
				assert (nu == null);
			else {
				if (nu.degree != i) {
					assert (nu != null);
					assert (nu.degree == i);

				}
			}
			i++;
		}
	}

	private Nullity(int n) {
		assert(n > 0);
		this.degree = n;
	}

	public static Nullity of(int n) {
		assert (n > 0);
		makeNullitiesThrough(n + 1);
		return nullities.get(n);
	}

	public String toString() {
		if (str == null) {
			StringBuffer sb = new StringBuffer();
			for (int i = 1; i <= degree; i++) {
				sb.append("+");
			}
			sb.append("null");
			str = sb.toString();
		}
		return str;
	}

	public Thing deplus() {
		if (degree == 1)
			return null;
		else {
			Nullity prev = Nullity.of(degree - 1);
			return prev;
		}
	}

	@Override
	public Thing enplus() {
		return Nullity.of(degree + 1);
	}

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
			case ChashCode:
				 return  this.ChashCode(methodName, args, evaller, frame, src); 
			default:
				Doom.internal("Method key structure broken", src, methodC);
				return null;
			}
		}
		return this.tryThingMethods(methodName, args,  src);
	}

	@Override
	public String typeString() {
		// TODO Auto-generated method stub
		return "nullity";
	}

	

}
