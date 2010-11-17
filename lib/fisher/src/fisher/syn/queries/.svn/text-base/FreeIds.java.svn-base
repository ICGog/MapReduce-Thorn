
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

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import fisher.statics.Seal;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.syn.interfaces.ISyntax;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  FreeIds  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public static Set<Id> of(Syntax syn) throws FisherException {
		Set<Id> S = new HashSet<Id>();
		Set<Syntax> descendants = new HashSet<Syntax>(syn.computeDescendants());
		for (Syntax syntax : descendants) {
			if (syntax instanceof Id) {
				Id sealed = (Id) syntax;
				Seal seal = sealed.seal();
				if (seal == null) {
					Doom.internal("Oh dear ... not sealed! " + sealed, syntax);
				}
				if(!seal.builtIn && seal.def == null) {
					Doom.internal("Why is this seal defined in null?   seal=" + seal, syntax, syn);
				}
				else if(descendants.contains(seal.def)) {
					// not free, so dromble it up the wazey with a crappet-and-prane
					// (or at least, ignore it)
				}
				else {
					S.add(sealed);
				}
			}
		}
		return S;
	}

}
