
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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import fisher.syn.ImportStmt;
import fisher.syn.core.Syntax;
import fisher.util.FisherException;
import fisher.util.SpecialCharacters;
	
public  class  PwnedSeal extends SealForModule  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public final String originalModuleName;
	public final ImportStmt importStmt;
	
	public PwnedSeal(Syntax def, String pwnedName, SealKind kind, Seal container, String originalModuleName,
			ImportStmt importStmt, ModuleStatic moduleStatic, java.util.List<Seal> contents) {
		super(def, pwnedName, kind, container, moduleStatic, false);
		this.originalModuleName = originalModuleName;
		this.importStmt = importStmt;
		this.moduleStatic = moduleStatic;
		this.contents = pwnAll(contents);
	}
	
	private List<Seal>  pwnAll(List<Seal> contentsOfOriginal) {
		List<Seal> L = new ArrayList<Seal>(contentsOfOriginal.size());
		for (Seal seal : contentsOfOriginal) {
			Seal peal = pwn(seal);
			L.add(peal);
		}
		return L;
	}
	
	Map<Seal, Seal> pwnCache = new HashMap<Seal, Seal>();
	
	public Seal pwn(Seal seal) {
		if (pwnCache.containsKey(seal)) return pwnCache.get(seal);
		Seal pwnedSeal = seal.pwnClone(this); // new Seal(seal.def, seal.str, seal.kind, this, seal.builtIn);
		pwnCache.put(seal, pwnedSeal);
		return pwnedSeal;
	}
	
	@Override
	public String toString() {
		return SpecialCharacters.SEAL +
			containerString() + 
			str + "“" + kind + "”=own(" + originalModuleName + /* "{" + (def == null ? "" : def.parent()) + "})" */ "){"+importStmt+"}";
	}
	

}
