
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

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.util.FisherException;

public abstract  class  AbstractRangeTh extends ThingImmutable  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	@Override
	public String typeString() {
		return "range";
	}

	@Override
	public AbstractRangeTh asRange(Syntax src) throws FisherException {
		return this;
	}
	
	@Override
	public boolean isRange() {
		return true;
	}
	
	

	@Override
	public Thing invokeMethod(String methodName, Thing[] args,  Syntax src)
			throws FisherException {
		return this.tryThingMethods(methodName, args,  src);
	}
	
	public abstract Thing low();
	public abstract Thing high();
	
	@Override
	public boolean canBeKey() {
		return EvalUtil.canBeKey(low()) && EvalUtil.canBeKey(high());
	}

//	public Internal_Success invokePat(Id patId, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
//	throws FisherException {
//		return this.patNotUnderstood(patId, args, evaller, frame, src);
//	}
}
