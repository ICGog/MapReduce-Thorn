
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

import fisher.runtime.Thing;
import fisher.syn.core.Syntax;
import fisher.util.FisherException;

public  class  ThornThrow extends FisherException  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public fisher.runtime.Thing thrown; 
	public fisher.syn.core.Syntax src;
	public ThornThrow(Thing thrown, Syntax src) {
		super();
		this.thrown = thrown;
		this.src = src;
	}
	public String toString() {
		return "" + thrown + " from " + src;
	} 
	

}
 
