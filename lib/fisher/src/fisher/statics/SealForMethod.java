
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

import fisher.syn.core.Syntax;

public  class  SealForMethod extends Seal  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public final MethodSig sig;

	public SealForMethod(Syntax def, String str, SealKind kind, Seal container, boolean builtIn, MethodSig sig) {
		super(def, str, kind, container, builtIn);
		this.sig = sig;
	}

	public String toString() {
		return super.toString() + "/" + sig.arity;
	}
}
