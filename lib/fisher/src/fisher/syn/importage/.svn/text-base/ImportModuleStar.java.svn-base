
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

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.Frame;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.Frameable;
import fisher.runtime.ModuleDynamicTh;
import fisher.runtime.Thing;
import fisher.statics.ModuleStatic;
import fisher.statics.Seal;
import fisher.statics.SealAlias;
import fisher.statics.SealForModule;
import fisher.syn.ImportStmt;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  ImportModuleStar extends AbstractImport  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public final ModuleStatic moduleStatic;
	public final SealForModule moduleSeal;
	private final List<Seal> membersToImport = new ArrayList<Seal>(3);
	
	
	public ImportModuleStar(ImportStmt src, ModuleStatic moduleStatic, SealForModule moduleSeal) {
		super(src);
		this.moduleStatic = moduleStatic;
		this.moduleSeal = moduleSeal;
	}

	public List<Seal> membersToImport() {return membersToImport;}
	
	public void addMemberToImport(Seal member) {
		membersToImport.add(member);
	}

	@Override
	public Thing eval(Framelike frame, Evaller evaller) throws FisherException {
//	
//		OK, here's a problem.  The instance is *always* the shared one.
//		However, 030f, it's *not* the shared one we want.
//		In scary fact, we want the one bound to B -- that is, to src.modName.ids.get(src.modName.ids.size()-1).seal()
//		(But that's not always right either -- that's only true for an aliased module.)
		
		ModuleDynamicTh instance;
		
		Seal modSeal = src.modName.ids.get(src.modName.ids.size()-1).seal();
		
		if (modSeal == null || !frame.hasSeal(modSeal)) {
			// If the module name isn't found in the env, make a new shared module.
			// In this case, src == "import FOO.*" for a FOO that isn't mentioned before.
			instance = evaller.getInstantiation(moduleStatic, true, src);
		}
		else {
			Thing thinstance = frame.RValue(modSeal, src);
			if (thinstance instanceof ModuleDynamicTh) {
				instance = (ModuleDynamicTh) thinstance;
			}
			else {
				Doom.runtime("Something that should have been a module, wasn't", src, "desired module= " + thinstance, "modSeal = "+ modSeal);
				return null;
			}
		}
		
		// So why aren't classes getting into the moduleDynamic?
		
		for (Seal memberSeal : membersToImport) {
			String nameInModule = 
				memberSeal instanceof SealAlias ? ((SealAlias)memberSeal).originalName : memberSeal.str();
			Frameable frameable = instance.baseValueByName(nameInModule, src);
			frame.store(memberSeal, frameable, src);
		}
		return EvalUtil.nip(src);
	}

}
