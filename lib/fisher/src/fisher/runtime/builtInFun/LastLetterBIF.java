
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.runtime.builtInFun;

import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.BuiltInFunctionTh;
import fisher.runtime.Thing;
import fisher.runtime.dist.ComponentThread;
import fisher.syn.core.Syntax;
import fisher.util.FisherException;

public  class  LastLetterBIF extends BuiltInFunctionTh  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	public static String name = "lastLetter";

	@Override
	public Thing apply(Thing[] args, Framelike ignoredFrame, Evaller evaller, Syntax src) throws FisherException {
		checkNumberOfArgs(0,0, name, args, evaller, ignoredFrame, src);
		return (Thing)ComponentThread.mine().lastLetter;
	}

}
