
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.util;

import fisher.eval.Evaller;
import fisher.runtime.Thing;

public  class  Security  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	private static boolean sandbox = false;
	
	public static void reset(int thisIsNotAThingSoThisFunCannotBeCalledFromThorn) {
		sandbox = false;
	}
	
	public static Thing goToSandbox() {
		sandbox = true;
		return null;
	}
	
	public static void sandbag(String... msgs) throws FisherException {
		if (sandbox) Doom.runtime("Security violation: " + Bard.sep(msgs, ","), Evaller.lastSyntax());
	}
	
}
