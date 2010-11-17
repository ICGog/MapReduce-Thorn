
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.runtime;

import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.syn.core.Syntax;
import fisher.util.FisherException;

public abstract  class  ThingBuiltIn extends Thing  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public ThingBuiltIn() {
		// TODO Auto-generated constructor stub
	}
	

	@Override
	public boolean canBeKey() {
		return true;
	}
	
	
	protected Thing ChashCode(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return IntTh.of(this.hashCode());
	}

}
