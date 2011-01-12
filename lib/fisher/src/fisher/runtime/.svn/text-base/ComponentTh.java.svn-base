
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
import fisher.runtime.dist.ComponentThread;
import fisher.runtime.dist.DistUtil;
import fisher.syn.core.Syntax;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  ComponentTh extends ThingImmutable  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	public final SiteTh site;
	public final long number;
	public final String name;
	public final long creationTime = System.currentTimeMillis(); // Used to avoid collisions
	public final long randomNumber = Bard.random.nextLong(); // Used to avoid collisions 
		
	
	public static ComponentTh of(SiteTh site, long number, String name) {
		return new ComponentTh(site, number, name);
	}
	
	public static ComponentTh mine(Syntax src) throws FisherException {
		if (Thread.currentThread() instanceof ComponentThread) {
			ComponentThread cth = (ComponentThread) Thread.currentThread();
			return cth.compTh;
		}
		else {
			Doom.runtime("Not in a component here.", src);
			return null;
		}
	}

	private ComponentTh(SiteTh site, long number, String name) {
		super();
		this.site = site;
		this.number = number;
		this.name = name;
	}
	
	public String toString() {
		return name; //+ "[" + site + "/" + number + "/" + randomNumber + "]"; 
	}
	public String details() {
		return name + "[" + site + "/" + number + "/" + randomNumber + "]"; 
	}
	
	@Override
	public boolean transmissible() {
		return true;
	}
	
	
	@Override
	public boolean equals(Object obj) {
		if (obj == null) return false;
		if (this.getClass() == obj.getClass()) {
			ComponentTh other = (ComponentTh) obj;
//			System.err.println("ComponentTh -- comparing");
//			System.err.println("... this: " + this.details() );
//			System.err.println("...other: " + other.details() );
			
			final boolean e1 = this.site.equals(other.site);
			final boolean e2 = e1 && this.number == other.number;
			final boolean e3 = e2 && this.creationTime == other.creationTime;
			final boolean e4 = e3 && this.randomNumber == other.randomNumber;
			return e4;
		}
		else return false;
	} 
	
	@Override
	public int hashCode() {
		return this.site.hashCode() ^ (int) this.number;
	}
	
	private final static int STR = 0;
	private final static int NUM = 1;
	private final static int Csite = 2;
	private final static int Csend = 3;
	
	private static Map<String, Integer> methodCode = new HashMap<String, Integer>();
	static {
		methodCode.put("num", NUM);
		methodCode.put("str", STR);
		methodCode.put("site", Csite);
		methodCode.put("<<<", Csend);
		methodCode.put("send", Csend);
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
				return this.tryThingMethods(methodName, args,    src);
			case Csite:
				return this.Csite(methodName, args, evaller, frame, src);
			case Csend:
				return this.Csend(methodName, args, evaller, frame, src);
			default:
				Doom.internal("Method key structure broken", src, methodC);
				return null;
			}
		}
		return this.tryThingMethods(methodName, args,    src);
	}
	
	private Thing Csend(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		DistUtil.send(ComponentTh.mine(src), this, args[0], args[1], src);
		return BoolTh.True;
	}
	
	
	private Thing Csite(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return this.site;
	}
	
	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		// TODO Auto-generated method stub
		return true;
	}

	@Override
	public String typeString() {
		// TODO Auto-generated method stub
		return "component";
	}

}
