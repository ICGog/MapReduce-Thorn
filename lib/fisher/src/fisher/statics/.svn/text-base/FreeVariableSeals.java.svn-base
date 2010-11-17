
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

import java.util.*;

import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.syn.queries.IdsAppearingIn;
import fisher.syn.*;

public  class  FreeVariableSeals  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public static Set<Seal> of(Syntax syn) {
		List<Syntax> descs = syn.computeDescendants();
		Set<Id> ids = IdsAppearingIn.of(syn);
		Set<Seal> seals = new HashSet<Seal>();
		for (Id id : ids) {
			Seal seal = id.seal();
			if (seal != null) {
				Syntax def = seal.def;
				if (descs.contains(def)) {
					// bound locally 
				} else {
					seals.add(seal);
				}
			}
			else {
				// Unsealed free variable ... will be caught sometime later, we hope.
			}
		}
		return seals;
	}
}
