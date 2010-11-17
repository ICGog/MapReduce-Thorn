
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

import fisher.syn.ImportStmt;
import fisher.syn.core.Syntax;
import fisher.util.SpecialCharacters;

public  class  SealForModule extends Seal  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public ModuleStatic moduleStatic;

	public SealForModule(Syntax def, String str, SealKind kind, Seal container, ModuleStatic moduleStatic, boolean builtIn) {
		super(def, str, kind, container, builtIn);
		this.moduleStatic = moduleStatic;
	}
	
	public PwnedSeal own(String pwnedName, ImportStmt importStmt) {
		return new PwnedSeal(this.def, pwnedName, this.kind, this.container, this.str(), importStmt, moduleStatic, this.contents);
	}
	
	public String toString() {
		return SpecialCharacters.SEAL + SpecialCharacters.MODULE + 
			containerString() + 
			str 
			// + "“" + kind + "”" 
			//+ "{" + (def == null ? "" : def.parent()) + "}"
			//+ "module{" + moduleStatic + "}"
			;
	}

}
