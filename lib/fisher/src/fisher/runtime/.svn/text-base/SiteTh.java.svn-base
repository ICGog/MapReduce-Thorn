
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
import fisher.runtime.dist.DistUtil;
import fisher.syn.core.Syntax;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  SiteTh extends ThingImmutable  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public final String nodeName;
	public final int port;
	
	public SiteTh(String nodeName, int port) {
		super();
		this.nodeName = nodeName;
		this.port = port;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj == null) return false;
		if (obj.getClass() == this.getClass()) {
			SiteTh other = (SiteTh) obj;
			return this.port == other.port && this.nodeName.equals(other.nodeName);
		}
		else return false;
	}
	
	@Override
	public int hashCode() {
		return this.port ^ this.nodeName.hashCode();
	}
	
	public boolean openStreamsAsObjectOutputStream() {
		return true;
	}
	public boolean openStreamsAsWriter() {
		return false;
	}
	
	private final static int STR = 0;
	private final static int NUM = 1;
	private final static int Chost = 2;
	private final static int Cport = 3;
	private final static int Csend = 4;
	
	private static Map<String, Integer> methodCode = new HashMap<String, Integer>();
	static {
		methodCode.put("num", NUM);
		methodCode.put("str", STR);
		methodCode.put("host", Chost);
		methodCode.put("port", Cport);
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
				return this.tryThingMethods(methodName, args,   src);
			case Chost:
				return this.Chost(methodName, args, evaller, frame, src);
			case Cport:
				return this.Cport(methodName, args, evaller, frame, src);
			case Csend:
				return this.Csend(methodName, args, evaller, frame, src);

			default:
				Doom.internal("Method key structure broken", src, methodC);
				return null;
			}
		}
		return this.tryThingMethods(methodName, args, src);
	}
	

	protected Thing Csend(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		DistUtil.send(ComponentTh.mine(src), this, args[0], args[1], src);
		return BoolTh.True;
	}

	
	private Thing Cport(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return IntTh.of(port);
	}
	
	private Thing Chost(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return StringTh.of(nodeName);
	}
	
	public String toString() {
		return "" + nodeName + ":" + port + "";
	}

	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public String typeString() {
		// TODO Auto-generated method stub
		return "site";
	}

}
