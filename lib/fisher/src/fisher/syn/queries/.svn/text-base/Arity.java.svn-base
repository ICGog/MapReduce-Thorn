
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

import fisher.syn.FunBody;
import fisher.syn.MonoBody;

public  class  Arity  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	// A little complicated by the fact that a funbody can declare more than one
	// arity at once
	public static boolean hasArity(int n, FunBody fb) {
		for (MonoBody mb : fb.funbodies) {
			int k = mb.formals.formals.size();
			if (k == n) return true;
		}
		return false;
	}
}
