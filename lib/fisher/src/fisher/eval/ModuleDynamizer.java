
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.eval;

import fisher.eval.interfaces.Framelike;
import fisher.runtime.ModuleDynamicTh;
import fisher.syn.Bind;
import fisher.syn.ClsDecl;
import fisher.syn.ComponentDecl;
import fisher.syn.FunDecl;
import fisher.syn.ImportStmt;
import fisher.syn.JavalyClassDecl;
import fisher.syn.JavalyFun;
import fisher.syn.MethDecl;
import fisher.syn.ModuleFileAlias;
import fisher.syn.ModuleFileImport;
import fisher.syn.ModuleFileMemberStmt;
import fisher.syn.ModuleFileVisibility;
import fisher.syn.VarDecl;
import fisher.syn.core.Syntax;
import fisher.syn.interfaces.ModuleFileMember;
import fisher.syn.visitor.ModuleFileMemberWalker;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  ModuleDynamizer implements ModuleFileMemberWalker<ModuleDynamicTh, FisherException>  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	private final Evaller evaller;
	
	
	private ModuleDynamizer(Evaller evaller) {
		super();
		this.evaller = evaller;
	}

	public static void fillInAllFields(ModuleDynamicTh instance, Evaller evaller, Syntax src) throws FisherException {
		ModuleDynamizer dynamizer = new ModuleDynamizer(evaller); 
		for (ModuleFileMember member : instance.moduleStatic.src.bits) {
	 		member.accept(dynamizer, instance);
		}
	}

	public void visit(Bind syn, ModuleDynamicTh minth) throws FisherException {
		Computer.doBind(syn, minth, evaller);
	}

	public void visit(VarDecl syn, ModuleDynamicTh minth) throws FisherException {
		Computer.doVarDecl(syn, syn.init, syn.var, minth, evaller);
	}

	public void visit(FunDecl syn, ModuleDynamicTh minth) throws FisherException {
		Computer.doFunDecl(syn, minth, evaller);
	}
	
	public void visit(JavalyFun syn, ModuleDynamicTh minth) throws FisherException {
		Computer.doJavalyFunDecl(syn, minth);
	}

	public void visit(MethDecl syn, ModuleDynamicTh minth) throws FisherException {
		Doom.notYet();
	}
	
	public void visit(JavalyClassDecl syn, ModuleDynamicTh minth) throws FisherException {
		Computer.doJavalyClassDecl(syn, minth);		
	}
	
	public void visit(ComponentDecl syn, ModuleDynamicTh minth) throws FisherException {
		Computer.doBind((Bind)syn.actualCode	, minth, evaller);		
	}



	public void visit(ClsDecl syn, ModuleDynamicTh minth) throws FisherException {
//		Computer.doClsDecl(syn, minth.frame); -- Why was this ".frame"?
		Computer.doClsDecl(syn, minth);

	}

	public void visit(ImportStmt syn, ModuleDynamicTh minth) throws FisherException {
		Computer.doImport(syn, minth);
	}

	public void visit(ModuleFileMemberStmt syn, ModuleDynamicTh minth) throws FisherException {
		Doom.notYet();

	}

	public void visit(ModuleFileImport syn, ModuleDynamicTh minth) throws FisherException {
		syn.imp.accept(this, minth);
	}

	public void visit(ModuleFileVisibility syn, ModuleDynamicTh minth) throws FisherException {
		// No effect

	}

	public void visit(ModuleFileAlias syn, ModuleDynamicTh minth) throws FisherException {
		Doom.notYet();

	}

}
