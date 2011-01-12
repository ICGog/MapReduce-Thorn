
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.statics;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;


import fisher.syn.Bind;
import fisher.syn.ClsDecl;
import fisher.syn.ComponentDecl;
import fisher.syn.FunDecl;
import fisher.syn.ImportStmt;
import fisher.syn.JavalyClassDecl;
import fisher.syn.JavalyFun;
import fisher.syn.Module;
import fisher.syn.ModuleFileImport;
import fisher.syn.ModuleFileVisibility;
import fisher.syn.PatVar;
import fisher.syn.VarDecl;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.syn.core.Visibility;
import fisher.syn.interfaces.ModuleFileMember;
import fisher.syn.queries.PatVars_Defined_By_Pattern;
import fisher.syn.visitor.VanillaVisitor;
import fisher.syn.visitor.VanillaWalker;
import fisher.test.SealTest;
import fisher.util.Bard;
import fisher.util.DangerLevel;
import fisher.util.Doom;
import fisher.util.FisherException;
import static fisher.statics.SealKind.*;

public  class  ExtractSealsFromModuleMember extends VanillaWalker<Object, FisherException>  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public final Seal moduleSeal;
	
	
	
	public ExtractSealsFromModuleMember(Seal moduleSeal) {
		super();
		this.moduleSeal = moduleSeal;
	}

	private Map<String, Seal> publics = new HashMap<String, Seal>();
	private Set<String> privates = new HashSet<String>();
	
	public Map<String, Seal> extract(Module syn) throws FisherException {
		syn.accept(this, null);
		
		SealForModule moduleSeal = (SealForModule) syn.name.last().seal();
		for (String priv : privates) {
			// A somewhat clumsy hack to privatize things.
			publics.remove(priv);
			final List<Seal> contents = moduleSeal.contents;
			List<Seal> removeThese = Bard.list();
			for (Seal seal : contents) {
				if (privates.contains(seal.str())) removeThese.add(seal);
			}
			contents.removeAll(removeThese);
		}
		return publics;
	}
	
	private  void putSeal(Seal seal, Syntax src) throws FisherException {
		String key = seal.str();
		if (publics.containsKey(key)) {
			src.flag(DangerLevel.ERROR, "Duplicate definition for " + key, "");
		}
		else {
			publics.put(key, seal);
		}
	}
	
	@Override
	public void visit(Syntax syn, Object dummy) throws FisherException {
		Doom.internal("No way to extract seals from this syntax has been programmed yet.", syn);
	}
	
	@Override
	public void visit(Module syn, Object dummy) throws FisherException {
		for (ModuleFileMember member : syn.bits) {
			((Syntax)member).accept(this, dummy);
		}
	}
	
	@Override
	public void visit(ModuleFileVisibility syn, Object dummy) throws FisherException {
		if (syn.vis == Visibility.PRIV) {
			privates.add(syn.id.str());	
		}
		else {
			Doom.notYet();
		}
	}
	
	
	@Override
	public void visit(ComponentDecl syn, Object dummy) throws FisherException {
		this.visit((Bind)syn.actualCode, null);
	}

	@Override
	public  void visit(VarDecl syn, Object dummy) throws FisherException {
		Seal seal = SealMaker.of(syn, moduleSeal);
		putSeal(seal, syn);
	}
	
	@Override
	public void visit(ClsDecl syn, Object dummy) throws FisherException {
		putSeal(SealMaker.of(syn, moduleSeal), syn);
	}
	
	@Override
	public void visit(FunDecl syn, Object dummy) throws FisherException {
		putSeal(SealMaker.of(syn, moduleSeal), syn);
	}
	
	@Override
	public void visit(JavalyFun syn, Object dummy) throws FisherException {
		putSeal(SealMaker.of(syn, moduleSeal), syn);
	}
	
	@Override
	public void visit(Bind syn, Object dummy) throws FisherException {
		List<PatVar> patvars = PatVars_Defined_By_Pattern.of(syn.pat);
		List<Seal> seals = SealMaker.of(patvars, moduleSeal);
		for (Seal seal : seals) {
			putSeal(seal, syn);
		}
	}
	
	@Override
	public void visit(ImportStmt syn, Object dummy) throws FisherException {
		// No seals from import.
	}
	
	@Override
	public void visit(ModuleFileImport syn,Object dummy) throws FisherException {
		// No seals come from import.
	}
	
	@Override
	public void visit(JavalyClassDecl syn, Object dummy) throws FisherException {
		putSeal(SealMaker.of(syn, moduleSeal), syn);
	}
}
