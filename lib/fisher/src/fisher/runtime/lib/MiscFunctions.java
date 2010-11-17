
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.runtime.lib;

import java.io.FileReader;
import java.io.IOException;

import fisher.eval.EvalUtil;
import fisher.run.Thorn;
import fisher.runtime.FileTh;
import fisher.runtime.IntTh;
import fisher.runtime.StringTh;
import fisher.runtime.Thing;
import fisher.util.FisherException;
import fisher.util.FisherSource;
import fisher.util.FisherSource.FromFile;

public  class  MiscFunctions  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public static Thing systemProperty(Thing string) throws FisherException {
		String s = EvalUtil.toString(string);
		final String property = System.getProperty(s);
		if (property == null) return null;
		return StringTh.of(property);
	}
	
	/**
	 * Get the file object of the current running script.
	 * 
	 * @return
	 * @throws FisherException
	 */
	public static Thing thisScript() throws FisherException{
		FisherSource src;
		if (Thorn.oneSrc != null) {
			src = Thorn.oneSrc;
		} else if(Thorn.spawnSrc != null){
			src = Thorn.spawnSrc;
		}
		else{
			src = Thorn.evalSrc;
		}
		java.io.File file = ((FromFile)src).file;
		return FileTh.of(file);
	} 
	
	public static long IDENTITY_counter = 0;
	public static Thing IDENTITY_counter() {
		IDENTITY_counter += 1;
		return IntTh.of(IDENTITY_counter);
	}
}
