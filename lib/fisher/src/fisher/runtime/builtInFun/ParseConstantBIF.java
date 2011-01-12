
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

import java.io.InputStream;
import java.io.StringReader;

import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.parser.FisherParser;
import fisher.parser.ParseException;
import fisher.runtime.BuiltInFunctionTh;
import fisher.runtime.Thing;
import fisher.syn.core.Syntax;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  ParseConstantBIF extends BuiltInFunctionTh  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	private static FisherParser parser = new FisherParser(new StringReader("This should never get read")); 
	
	@Override
	public Thing apply(Thing[] args, Framelike ignoredFrame, Evaller evaller, Syntax src) throws FisherException {
		checkNumberOfArgs(1, 1, "parseConstant", args, evaller, ignoredFrame, src);
		String s = args[0].asString(src);
		StringReader sr = new StringReader(s);
		parser.ReInit(sr);
		try {
			Thing thing = parser.ReadThing();
			return thing;
		} catch (ParseException e) {
			Doom.runtime("Error parsing constant: " + s + " caused " + e, src);
			return null;
		}
	}

}
