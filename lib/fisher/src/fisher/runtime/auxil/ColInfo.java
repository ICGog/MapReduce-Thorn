
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.runtime.auxil;

import fisher.runtime.TableTh;
import fisher.syn.Cmd;
import fisher.syn.TypeConstraints;
import fisher.syn.core.ColAccess;
import fisher.syn.core.ColSpecial;

public  class  ColInfo  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public final String name;
	public final ColAccess access;
	public final ColSpecial special;
	public final int pos;
	public final Cmd optInit;
	public final TypeConstraints typeConstraints;
	public ColInfo(String name, ColAccess access, ColSpecial special, int pos, Cmd optInit, TypeConstraints typeConstraints) {
		super();
		this.name = name;
		this.access = access;
		this.special = special;
		this.pos = pos;
		this.optInit = optInit;
		this.typeConstraints = typeConstraints;
	}
	
	public String toString() {
		return special + " " + access + " " + name;
	}
	
}
