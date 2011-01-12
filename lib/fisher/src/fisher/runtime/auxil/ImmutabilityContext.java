
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.runtime.auxil;

import java.util.HashSet;
import java.util.Set;

import fisher.runtime.Thing;

public  class  ImmutabilityContext  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	private Set<Thing> assumedImmutable = new HashSet<Thing>();
	public void see(Thing thing) {
		assumedImmutable.add(thing);
	}
	public boolean seen(Thing thing){
		return assumedImmutable.contains(thing);
	}
}
