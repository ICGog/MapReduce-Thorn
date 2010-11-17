
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.syn.importage;

import java.util.ArrayList;
import java.util.List;

import fisher.eval.Evaller;
import fisher.eval.Frame;
import fisher.runtime.Thing;
import fisher.statics.ModuleStatic;
import fisher.statics.Seal;
import fisher.syn.ImportStmt;
import fisher.util.FisherException;

public abstract  class  AbstractImport  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	public final ImportStmt src;
	public List<Seal> modulesToImport = new ArrayList<Seal>(0);
	
	public abstract Thing eval(fisher.eval.interfaces.Framelike frame, Evaller evaller) throws FisherException;
	public AbstractImport(ImportStmt src) {
		super();
		this.src = src;
	}
}
