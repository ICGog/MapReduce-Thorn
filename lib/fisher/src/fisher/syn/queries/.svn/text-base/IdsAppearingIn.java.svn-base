
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
import java.util.*;

import fisher.syn.core.Id;
import fisher.syn.core.Syntax;

public  class  IdsAppearingIn  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public static Set<Id> of(Syntax syn) {
		List<Syntax> descs = syn.computeDescendants();
		Set<Id> ids = new HashSet<Id>();
		for (Syntax d : descs) {
			if (d instanceof Id) {
				Id id = (Id) d;
				ids.add(id);
			}
		}
		return ids;
	}
}
