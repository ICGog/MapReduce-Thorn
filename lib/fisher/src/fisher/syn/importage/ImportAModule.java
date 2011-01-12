
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

import java.util.List;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.Frame;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.ModuleDynamicTh;
import fisher.runtime.Thing;
import fisher.statics.ModuleStatic;
import fisher.statics.Seal;
import fisher.syn.ImportStmt;
import fisher.syn.core.Id;
import fisher.util.FisherException;

public  class  ImportAModule extends AbstractImport  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public final Id MOD;
	public final Id MOOD;
	public final ModuleStatic moduleStatic;
	public final Seal moduleSeal; 
	
	public ImportAModule(ImportStmt src, Id mod, Id mood, ModuleStatic moduleStatic, Seal moduleSeal) {
		super(src);
		MOD = mod;
		MOOD = mood;
		this.moduleStatic = moduleStatic;
		this.moduleSeal = moduleSeal;
	}

	@Override
	public Thing eval(Framelike frame, Evaller evaller) throws FisherException {
		ModuleDynamicTh instance = evaller.getInstantiation(moduleStatic, true, src);
		frame.store(moduleSeal, instance, src);
		return EvalUtil.nip(src);
	}

	public String toString() {
		return "import_a_module " + MOOD + " = " + MOD + ";"; 
	}
}
