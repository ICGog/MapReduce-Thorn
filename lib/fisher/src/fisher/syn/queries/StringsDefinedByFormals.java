
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.syn.queries;

import fisher.syn.Formals;
import fisher.syn.Pat;
import fisher.syn.PatVar;
import fisher.util.DangerLevel;
import fisher.util.Doom;
import fisher.util.FisherException;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public  class  StringsDefinedByFormals  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public static Set<String> of(Formals formals) throws FisherException {
 		if (formals == null) return Collections.EMPTY_SET;
		
		Set<String> S = new HashSet<String>();
		for(Pat p : formals.formals) {
			List<PatVar> pv = PatVars_Defined_By_Pattern.of(p);
			for (PatVar patVar : pv) {
				String v = patVar.id.str();
				if (S.contains(v)) {
					formals.flag(DangerLevel.ERROR, "Duplicate definition for " + v, "");
				}
				else {
					S.add(v);
				}
			}
		}
		return S;
	}
}
