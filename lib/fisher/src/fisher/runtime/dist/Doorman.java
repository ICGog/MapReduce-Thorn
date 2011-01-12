
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.runtime.dist;

public  class  Doorman extends Thread  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	final public SiteData siteImpl;

	public Doorman(SiteData site) {
		super();
		this.siteImpl = site;
	}
	
	public String toString() {
		return "Doorman{" + siteImpl.siteTh + "}";
	}
	
	@Override
	public void run() {
		System.err.println("Doorman: run.  and that's all.");
	}

}
