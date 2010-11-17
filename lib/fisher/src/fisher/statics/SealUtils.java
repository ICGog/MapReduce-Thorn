
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

import java.util.HashSet;
import java.util.Set;

public  class  SealUtils  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	private SealUtils(){}
	
	/**
	 * SealUtils keeps some caches.  clear() clears those caches.
	 * TODO: After the seal-computing phase, you might want to call
	 * clear() -- before evaluation anyways -- to dump some memory.
	 * 
	 */
	public static void clear() {
		ALL_SEALS.clear();
	}
	
	public static Set<Seal> ALL_SEALS = new HashSet<Seal>();
	
	public static void register(Seal seal) {
		ALL_SEALS.add(seal);
	}
	
	
}
