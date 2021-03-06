
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

import java.util.Scanner;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.BuiltInFunctionTh;
import fisher.runtime.StringTh;
import fisher.runtime.Thing;
import fisher.syn.core.Syntax;
import fisher.util.FisherException;

public  class  ReadlnBIF extends BuiltInFunctionTh  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	
	public static Scanner inscanner = new Scanner(System.in); 
	@Override
	public Thing apply(Thing[] args, Framelike ignoredFrame, Evaller evaller, Syntax src) throws FisherException {
		for (Thing thing : args) {
			System.out.print(EvalUtil.toString(thing));
		}
		String s = inscanner.nextLine();
		return StringTh.of(s);
	}

}
