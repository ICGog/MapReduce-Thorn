
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
import java.util.List;
import java.util.Map;

import fisher.syn.AnonObj;
import fisher.syn.Bind;
import fisher.syn.ClsCtorDef;
import fisher.syn.ClsDecl;
import fisher.syn.ImportStmt;
import fisher.syn.MethDecl;
import fisher.syn.PatTypeTest;
import fisher.syn.PatVar;
import fisher.syn.VarDecl;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.syn.interfaces.Classlike;
import fisher.syn.queries.PatVars_Defined_By_Pattern;
import fisher.syn.visitor.VanillaWalker;
import fisher.util.Bard;
import fisher.util.DangerLevel;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  ExtractSealsFromClsDecl extends VanillaWalker<Object, FisherException>  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	final Syntax src;
	final Env classEnv;
	final SealForClass container;
	final Map<String, Seal> sealsOfNonMethods;
	final Sealant sealant;

	private ExtractSealsFromClsDecl(Syntax src, Env classEnv, SealForClass container, Map<String, Seal> seals,
			Sealant sealant) {
		super();
		this.src = src;
		this.classEnv = classEnv;
		this.container = container;
		this.sealsOfNonMethods = seals;
		this.sealant = sealant;
	}

	public ExtractSealsFromClsDecl(Classlike src, Env classEnv, Sealant sealant) {
		this((Syntax) src, classEnv, (SealForClass) classEnv.container, new HashMap<String, Seal>(), sealant);
	}

	//	public ExtractSealsFromClsDecl(ClsDecl clsDecl, Env classEnv, Sealant sealant) {
	//		this(clsDecl, classEnv, (SealForClass) classEnv.container, new HashMap<String, Seal>(), sealant);
	////		this.src = clsDecl;
	////		this.classEnv = classEnv;
	////		this.container = (SealForClass) classEnv.container;
	////		this.seals = new HashMap<String, Seal>();
	////		this.sealant = sealant;
	//	}

	@Override
	public void visit(Syntax syn, Object whatever) throws FisherException {
		Doom.internal("No way to extract seals from this syntax has been programmed yet.", syn);
	}

	private void remember(String str, Seal seal, Syntax src) throws FisherException {
		if (sealsOfNonMethods.containsKey(str)) {
			src.flag(DangerLevel.ERROR, "Duplicate definition for " + str, "", str, seal, src, this.src, classEnv,
					container, sealsOfNonMethods);
		} else {
			sealsOfNonMethods.put(str, seal);
			this.classEnv.define(str, seal);
		}
	}

	public void visit(fisher.syn.ClsPatDef syn, Object gorfless) throws FisherException {
		Id id = syn.id;
		Seal idSeal = SealMaker.of(syn, container);
		remember(id.str(), idSeal, syn);
	}

	@Override
	public void visit(Bind syn, Object arg) throws FisherException {
		List<PatVar> patvars = PatVars_Defined_By_Pattern.of(syn.pat);
		if (patvars.size() == 1 && syn.exp == null) {
			//special case: 'val v;' in a class at the top level
			Id id = patvars.get(0).id;
			List<Id> typeIds;
			if (syn.pat instanceof PatVar) {
				PatVar pv = (PatVar) syn.pat;
				typeIds = Bard.list();
			}
			else if (syn.pat instanceof PatTypeTest) {
				PatTypeTest ptt = (PatTypeTest) syn.pat;
				if (ptt.subpat instanceof PatVar) {
					PatVar pv = (PatVar) ptt.subpat;
					//OK
					if (ptt.shouldBe.ids.size() != 1) {
						Doom.internal("Can't currently handle 'var x:A.B;'", syn);
					}
					typeIds = Bard.list(ptt.shouldBe.last());
				}
				else {
					Doom.internal("Bad kind of var binding", syn);
					return;
				}
			}
			else {
				Doom.internal("Should never happen.", syn);
				typeIds = null;
			}
			Seal seal = SealMaker.ofUninitVal(id, syn, container, typeIds);
			remember(id.str(), seal, syn);
		} else if (syn.exp == null) {
			syn.flag(DangerLevel.ERROR, "Uninitialized class val's must be defined as: 'val v;'",
					"(Only one per definition.)");
		} else {
			List<Seal> seals = SealMaker.of(patvars, container);
			for (Seal seal : seals) {
				remember(seal.str, seal, syn);
			}
		}
	}

	public void visit(VarDecl syn, Object rubberDuckie) throws FisherException {
		Seal seal = SealMaker.of(syn, container);
		remember(seal.str, seal, syn);
	}

	@Override
	public void visit(MethDecl syn, Object arg) throws FisherException {
		Seal seal = SealMaker.of(syn, container);
		assert seal != null;
		// remember(seal.str, seal, syn);

		syn.name.setSeal(seal, syn);
	}

	@Override
	public void visit(ImportStmt syn, Object arg) throws FisherException {
		// It is convenient to import stuff in phase 1.
		sealant.sealThus(classEnv, syn);
	}

	@Override
	public void visit(ClsCtorDef syn, Object arg) throws FisherException {
		// I don't think we do anything for this.
	}

}
