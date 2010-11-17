
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

import fisher.syn.core.Id;
import fisher.syn.core.Syntax;

public  class  SealForVal extends SealTyped  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	final public boolean needsInit;

	public SealForVal(Syntax def, String str, SealKind kind, Seal container, boolean needsInit, java.util.List<Id> types) {
		super(def, str, kind, container, false, types);
		this.needsInit = needsInit;
	}
	
}
