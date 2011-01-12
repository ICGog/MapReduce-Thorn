
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
import fisher.util.SpecialCharacters;

public  class  SealAlias extends Seal  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public final String originalName;
	public final Seal originalSeal;

	public SealAlias(Syntax def, String str, SealKind kind, Seal container, String originalName, Seal originalSeal) {
		super(def, str, kind, container, originalSeal.builtIn);
		this.originalName = originalName;
		this.originalSeal = originalSeal;
	}
	
	public String toString() {
		return SpecialCharacters.SEAL +
			containerString() + 
			originalSeal.str 
			+ SpecialCharacters.ALIAS + str;
			//+ "“" + kind + "”" + "{" + (def == null ? "" : def.parent()) + "}(alias for " + originalName +")";
	}
	
	public Seal dealias() {
		return originalSeal;
	}
	
	@Override
	public boolean isAnAliasFor(Seal other) {
		return other.isAnAliasFor(this.originalSeal);
	}
}
