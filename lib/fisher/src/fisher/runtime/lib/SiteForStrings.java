
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

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.BoolTh;
import fisher.runtime.ComponentTh;
import fisher.runtime.SiteTh;
import fisher.runtime.Thing;
import fisher.runtime.ThingExtended;
import fisher.runtime.dist.DistUtil;
import fisher.syn.core.Syntax;
import fisher.util.FisherException;

public  class  SiteForStrings extends SiteTh  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	public static Thing create(Thing nodeNameTh, Thing portTh) throws FisherException{
		Syntax here = Evaller.lastSyntax();
		String nodeName = nodeNameTh.asString(here);
		int port = portTh.asJavaInt(here);
		return new SiteForStrings(nodeName, port);
	}

	public SiteForStrings(String nodeName, int port) {
		super(nodeName, port);
	}
	
	@Override
	public String typeString() {
		return "siteForString";
	}
	
	public boolean openStreamsAsObjectOutputStream() {
		return false;
	}
	public boolean openStreamsAsWriter() {
		return true;
	}

	protected void sendString(String s, Syntax src, Evaller evaller, Framelike frame) throws FisherException {
		DistUtil.sendString(s, this, null, src);
	}
	
	
	
	
	@Override
	protected Thing Csend(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(1, 2, methodName, args, evaller, frame, src);
		String string = EvalUtil.toString(args[0]);
		this.sendString(string, src, evaller, frame);
		return BoolTh.True;
	}
	

	
	
}
