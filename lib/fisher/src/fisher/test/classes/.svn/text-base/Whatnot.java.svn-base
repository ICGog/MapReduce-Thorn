
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.test.classes;

import fisher.runtime.ListTh;
import fisher.runtime.StringTh;
import fisher.runtime.Thing;
import fisher.runtime.ThingExtended;
import fisher.util.FisherException;

public  class  Whatnot extends ThingExtended   { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	public Thing dim() throws FisherException{
		return ListTh.of(a,b);
	}
	
	public Thing un(Thing x) throws fisher.util.FisherException {
		return ListTh.of(a,x,b);
	}
	
	public Thing a,b;
	public Whatnot() {
		a = StringTh.of("a?");
		b = StringTh.of("b?");
	}
	
	public Whatnot(Thing a, Thing b) { this.a = a; this.b = b; }
	

}
