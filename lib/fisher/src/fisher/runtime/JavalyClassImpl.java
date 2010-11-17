
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

import fisher.runtime.auxil.ImmutabilityContext;
import fisher.syn.JavalyClassDecl;
import fisher.util.FisherException;
import fisher.util.Security;

public  class  JavalyClassImpl extends ThingBuiltIn  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public final Class cls;
	public final JavalyClassDecl decl;
	
	
	public JavalyClassImpl(Class cls, JavalyClassDecl decl) throws FisherException{
		super();
		Security.sandbag("No javaly classes allowed.");
		this.cls = cls;
		this.decl = decl;
	}

	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		return false;
	}

	@Override
	public String typeString() {
		return "java class";
	}
	
	public String toString() {
		return cls.toString();
	}

}
