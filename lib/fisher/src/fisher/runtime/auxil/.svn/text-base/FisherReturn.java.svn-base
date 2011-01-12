
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

public  class  FisherReturn extends FisherException  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public Thing thing;
	public Syntax src;
	
	public FisherReturn(Thing thing, Syntax src) {
		super("return");
		this.thing = thing;
		this.src = src;
	}
	
	public String toString() {
		return "returning " + thing + "\nfrom " + src + "\n"+ super.toString();
		
	}
	
	
}
