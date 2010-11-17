
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
import fisher.runtime.Frameable;
import fisher.runtime.ModuleDynamicTh;
import fisher.runtime.Thing;
import fisher.statics.ModuleStatic;
import fisher.statics.Seal;
import fisher.syn.ImportStmt;
import fisher.syn.core.Id;
import fisher.util.FisherException;

public  class  ImportAField extends AbstractImport  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public final Id MOD;
	public final Id mood;
	public final Id doom;
	public final ModuleStatic moduleStatic;
	public final Seal moduleSeal;
	public final boolean moduleShouldBeLoadedAlready; 
	public final Seal targetSeal;
	public final Seal sealOfFieldOfModule;

	
	
	public ImportAField(ImportStmt src, Id mod, Id mood, Id doom, ModuleStatic moduleStatic, Seal moduleSeal, boolean moduleShouldBeLoadedAlready, 
			Seal sealOfFieldOfModule, Seal targetSeal) {
		super(src);
		MOD = mod;
		this.mood = mood;
		this.doom = doom;
		this.moduleStatic = moduleStatic;
		this.moduleSeal = moduleSeal;
		this.moduleShouldBeLoadedAlready = moduleShouldBeLoadedAlready;
		this.sealOfFieldOfModule = sealOfFieldOfModule;
		this.targetSeal = targetSeal;
	}



	@Override
	public Thing eval(Framelike frame, Evaller evaller) throws FisherException {
		ModuleDynamicTh instance;
		if (this.moduleShouldBeLoadedAlready) {
			instance = (ModuleDynamicTh) frame.RValue(moduleSeal, src);
		}
		else {
			instance = evaller.getInstantiation(moduleStatic, true, src);
			// Store the instance in the frame, thus effectively loading it.
			frame.store(moduleSeal, instance, src);
		}
		Frameable frameable = instance.baseValue(sealOfFieldOfModule, src);
		frame.store(targetSeal, frameable, src);
		return EvalUtil.nip(src);		
	}

}
