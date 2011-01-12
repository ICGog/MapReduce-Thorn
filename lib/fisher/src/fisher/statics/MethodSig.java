
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.statics;

public  class  MethodSig  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	public static final MethodSig INIT = new MethodSig("init", 0);
	
	public final String name;
	public final int arity;
	public MethodSig(String name, int arity) {
		super();
		this.name = name;
		this.arity = arity;
	}
	@Override
	public boolean equals(Object obj) {
		if (obj == null) return false;
		if (obj instanceof MethodSig) {
			MethodSig ms = (MethodSig) obj;
			return (ms.arity == this.arity) && (ms.name.equals(this.name));
		}
		else return false;
	}
	public String toString() {
		return name + "/" + arity;
	}
	@Override
	public int hashCode() {
		return name.hashCode() ^ arity;
	}
}
