
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

import java.util.HashMap;
import java.util.Map;

import fisher.runtime.BuiltInFunctionTh;
import fisher.runtime.Thing;
import fisher.statics.PredefinedIdentifiers.Predef;
import fisher.util.Doom;
import fisher.util.FisherException;
import fisher.util.FisherRuntimeException;

public  class  BIFfer  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	private static Map<Class, BuiltInFunctionTh> bifCache = new HashMap<Class, BuiltInFunctionTh>();

	public static Thing of(Predef pd) throws FisherException {
		if (pd.cls != null) {
			if (bifCache.containsKey(pd.cls)) {
				return bifCache.get(pd.cls);
			} else {
				try {
					BuiltInFunctionTh bif = (BuiltInFunctionTh) pd.cls.newInstance();
					bifCache.put(pd.cls, bif);
					return bif;
				} catch (InstantiationException e) {
					throw new FisherRuntimeException(e);
				} catch (IllegalAccessException e) {
					throw new FisherRuntimeException(e);
				}
			}
		}
		else if (pd.val != null) {
			return pd.val;
		}
		else {
			Doom.internal("Unknown predef", null, pd);
			return null;
		}
	}
}
