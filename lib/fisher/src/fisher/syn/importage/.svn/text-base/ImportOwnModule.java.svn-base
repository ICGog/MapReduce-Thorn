
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

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.Frame;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.ModuleDynamicTh;
import fisher.runtime.Thing;
import fisher.statics.ModuleStatic;
import fisher.statics.PwnedSeal;
import fisher.syn.ImportStmt;
import fisher.util.FisherException;

public  class  ImportOwnModule extends AbstractImport  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public final ModuleStatic moduleStatic;
	public final PwnedSeal sealToCallIt;
	
	
	
	public ImportOwnModule(ImportStmt src, ModuleStatic moduleStatic, PwnedSeal sealToCallIt) {
		super(src);
		this.moduleStatic = moduleStatic;
		this.sealToCallIt = sealToCallIt;
	}



	@Override
	public Thing eval(Framelike frame, Evaller evaller) throws FisherException {
		ModuleDynamicTh instance = evaller.getInstantiation(moduleStatic, false, src);
		frame.store(sealToCallIt, instance, src);
		return EvalUtil.nip(src);
	}

}
