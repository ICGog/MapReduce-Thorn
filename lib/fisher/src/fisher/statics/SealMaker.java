
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

import static fisher.statics.SealKind.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import fisher.syn.AnonObj;
import fisher.syn.AssignToFieldOfSubscripted;
import fisher.syn.AssignTofield;
import fisher.syn.AsyncDecl;
import fisher.syn.AsyncStmt;
import fisher.syn.ClassFormal;
import fisher.syn.ClsDecl;
import fisher.syn.ClsPatDef;
import fisher.syn.FieldRef;
import fisher.syn.FunDecl;
import fisher.syn.JavalyClassDecl;
import fisher.syn.JavalyFun;
import fisher.syn.MethDecl;
import fisher.syn.MethodCall;
import fisher.syn.Module;
import fisher.syn.MonoBody;
import fisher.syn.PatExtract;
import fisher.syn.PatMethodCall;
import fisher.syn.PatRecordField;
import fisher.syn.PatVar;
import fisher.syn.Probe;
import fisher.syn.Spawn;
import fisher.syn.SuperCall;
import fisher.syn.SyncDecl;
import fisher.syn.SyncStmt;
import fisher.syn.VarDecl;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.syn.visitor.VanillaVisitor;
import fisher.util.DangerLevel;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  SealMaker extends VanillaVisitor<Seal, Seal, FisherException>  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	private static SealMaker sealmaker = new SealMaker();

	public static Seal of(Syntax syn, Seal container) throws FisherException {
		return syn.accept(sealmaker, container);
	}

	public static Seal ofIdInDeadCode(String name) {
		return new Seal(null, name, SealKind.DEAD, null, false);
	}

	public static Seal ofPredefFun(String name) {
		return new Seal(null, name, SealKind.VAL, null, true);
	}

	public static Seal ofLoop(Id id, Syntax src, Seal container) throws FisherException {
		Seal seal = new Seal(src, id.str(), LOOP_NAME, container, false);
		id.setSeal(seal, src);
		return seal;
	}

	public static Seal ofUnlabelledLoop(Syntax src, Seal container) throws FisherException {
		Seal seal = new Seal(src, "-unlabelled-", LOOP_NAME, container, false);
		return seal;
	}

	public static Seal ofIt(Syntax src, Seal container) throws FisherException {
		Doom.internal("I thought 'it' was disabled.", src);
		Seal seal = new Seal(src, "it", IT, container, false);
		return seal;
	}

	public static Seal ofFieldName(Id id, Syntax src, Seal container) throws FisherException {
		Seal seal = new Seal(src, id.str(), FIELD_NAME, container, false);
		id.setSeal(seal, src);
		return seal;
	}

	public static Seal ofColumn(Id id, Syntax src) throws FisherException {
		Seal seal = new Seal(src, id.str(), COLUMN, null, false);
		id.setSeal(seal, src);
		return seal;
	}

	public static SealForVal ofUninitVal(Id id, Syntax src, Seal container, List<Id> typeIds) throws FisherException {
		SealForVal s = new SealForVal(src, id.str(), VAL, container, true, typeIds);
		id.setSeal(s, src);
		return s;
	}

	public static List<Seal> of(List<? extends Syntax> syns, Seal container) throws FisherException {
		List<Seal> L = new ArrayList<Seal>(syns.size());
		for (Syntax syn : syns) {
			L.add(of(syn, container));
		}
		return L;
	}

	public static SealForPatFormal ofPatFormal(Id id, ClsPatDef patDef, SealForClass container) throws FisherException {
		return new SealForPatFormal(patDef, id.str(), container);
	}

	public static Seal being_tested(Syntax syn, String str) {
		return new Seal(syn, str, BEING_TESTED, null, false);
	}

	@Override
	public Seal visit(Syntax syn, Seal arg) throws FisherException {
		Doom.internal("Attempting to make a seal from something that doesn't produce them: a " + syn.getClass(), syn);
		return null;
	}

	@Override
	public Seal visit(VarDecl syn, Seal container) throws FisherException {
		if (syn.var.seal() != null)
			return syn.var.seal();
		Seal seal = new SealTyped(syn, syn.var.str(), VAR, container, false, syn.typeConstraints.ids());
		syn.var.setSeal(seal, syn);
		return seal;
	}

	@Override
	public Seal visit(ClassFormal syn, Seal container) throws FisherException {
		if (syn.name.seal() != null)
			return syn.name.seal();
		Seal seal = new SealTyped(syn, syn.name.str(), syn.isVar ? VAR : VAL, container, false, syn.typeConstraints
				.ids());
		syn.name.setSeal(seal, syn);
		return seal;
	}

	@Override
	public Seal visit(ClsDecl syn, Seal container) throws FisherException {
		if (syn.name.seal() != null)
			return syn.name.seal();
		SealForClass seal = new SealForClass(syn, syn.name.str(), CLASS, container, false);
		syn.name.setSeal(seal, syn);
		syn.setClassSeal(seal);
		return seal;
	}

	public Seal visit(FunDecl syn, Seal container) throws FisherException {
		if (syn.name.seal() != null)
			return syn.name.seal();
		Seal seal = new Seal(syn, syn.name.str(), FUN, container, false);
		syn.name.setSeal(seal, syn);
		return seal;
	}

	@Override
	public Seal visit(Probe syn, Seal arg) throws FisherException {
		Seal seal = new Seal(syn, syn.toString(), PROBE, arg, false);
		if (syn.id != null)
			syn.id.setSeal(seal, syn);
		return seal;
	}

	@Override
	public Seal visit(PatVar syn, Seal container) throws FisherException {
		// Sometimes we'll visit a pattern twice -- 
		// e.g., module MO { a = 1; }
		// We'll visit 'a' once when we get the statics that the module exports, 
		// And again when we sealant the whole module.
		if (syn.id.seal() != null)
			return syn.id.seal();
		Seal seal = new Seal(syn, syn.id.str(), VAL, container, false);
		syn.id.setSeal(seal, syn);
		return seal;
	}

	@Override
	public Seal visit(Module syn, Seal arg) throws FisherException {
		Doom.internal("This is not the right way to make seals for modules", syn, arg);
		return null;
	}

	public static SealForModule forModule(Module syn, Seal container, ModuleStatic moduleStatic) throws FisherException {
		if (syn.name.ids.size() != 1) {
			Doom.notYet();
			Doom.internal("I currently only support single-ID module names", syn);
			return null;
		} else {
			Id id = syn.name.ids.get(0);
			Seal idSeal = id.seal();
			if (idSeal != null) {
				if (idSeal instanceof SealForModule) {
					SealForModule sfm = (SealForModule) idSeal;
					return sfm;

				} else {
					syn.flag(DangerLevel.ERROR, idSeal.str() + " is not a module.", "", id, idSeal);
					return null;
				}
			}
			SealForModule seal = new SealForModule(syn, id.str(), MODULE, container, moduleStatic, false);
			syn.moduleSeal = seal;
			id.setSeal(seal, syn);
			return seal;
		}
	}

	@Override
	public Seal visit(ClsPatDef syn, Seal container) throws FisherException {
		if (syn.id.seal() != null)
			return syn.id.seal();
		Seal seal = new Seal(syn, syn.id.str(), PAT_SEAL, container, false);
		syn.id.setSeal(seal, syn);
		return seal;
	}

	@Override
	public Seal visit(MethDecl syn, Seal arg) throws FisherException {
		if (syn.name.seal() != null)
			return syn.name.seal();

		int arity = -1;
		for (MonoBody mb : syn.funbody.funbodies) {
			int mbarity = mb.formals.formals.size();
			if (arity == -1) {
				arity = mbarity;
			} else if (arity != mbarity) {
				syn.flag(DangerLevel.ERROR, "All clauses of a method declaration must have the same arity", syn.name
						+ " started off looking like a function of " + arity + " arguments, but a later clause has "
						+ mbarity);
			} else {
				// all is well.
			}
		}
		MethodSig sig = new MethodSig(syn.name.str(), arity);

		Seal seal = new SealForMethod(syn, syn.name.str(), METHOD, arg // WAS: arg.container
				, false, sig);
		syn.name.setSeal(seal, syn);

		return seal;
	}

	@Override
	public Seal visit(AssignTofield syn, Seal container) throws FisherException {
		if (syn.field.seal() != null)
			return syn.field.seal();
		Seal seal = new Seal(syn, syn.field.str(), FIELD_REF, container, false);
		syn.field.setSeal(seal, syn);
		return seal;
	}

	@Override
	public Seal visit(AssignToFieldOfSubscripted syn, Seal container) throws FisherException {
		if (syn.field.seal() != null)
			return syn.field.seal();
		Seal seal = new Seal(syn, syn.field.str(), FIELD_REF, container, false);
		syn.field.setSeal(seal, syn);
		return seal;
	}

	@Override
	public Seal visit(FieldRef syn, Seal container) throws FisherException {
		if (syn.field.seal() != null)
			return syn.field.seal();
		Seal seal = new Seal(syn, syn.field.str(), FIELD_REF, container, false);
		syn.field.setSeal(seal, syn);
		return seal;
	}

	@Override
	public Seal visit(MethodCall syn, Seal container) throws FisherException {
		// This code is parallel to that for PatMethodCall.
		if (syn.method.seal() != null)
			return syn.method.seal();
		Seal seal = new Seal(syn, syn.method.str(), METHOD_REF, container, false);
		syn.method.setSeal(seal, syn);
		return seal;
	}

	@Override
	public Seal visit(PatMethodCall syn, Seal container) throws FisherException {
		// This code is parallel to that for MethodCall.
		if (syn.methodName.seal() != null)
			return syn.methodName.seal();
		Seal seal = new Seal(syn, syn.methodName.str(), METHOD_REF, container, false);
		syn.methodName.setSeal(seal, syn);
		return seal;
	}

	@Override
	public Seal visit(PatExtract syn, Seal container) throws FisherException {
		Doom.internal("I wonder if this ever happens?" , syn);
		return null;
		/*
		if (syn.patname.seal() != null)
			return syn.patname.seal();
		Seal seal = new Seal(syn, syn.patname.str(), PAT_REF, container, false);
		syn.patname.setSeal(seal, syn);
		return seal;
		*/
	}

	@Override
	public Seal visit(SuperCall syn, Seal container) throws FisherException {
		if (syn.method.seal() != null)
			return syn.method.seal();
		Seal seal = new Seal(syn, syn.method.str(), METHOD_REF, container, false);
		syn.method.setSeal(seal, syn);
		return seal;
	}

	@Override
	public Seal visit(PatRecordField syn, Seal container) throws FisherException {
		if (syn.id.seal() != null)
			return syn.id.seal();
		Seal seal = new Seal(syn, syn.id.str(), FIELD_REF, container, false);
		syn.id.setSeal(seal, syn);
		return seal;
	}

	@Override
	public Seal visit(AnonObj syn, Seal arg) throws FisherException {
		SealForClass seal = new SealForClass(syn, "(object)", ANON_OBJ_CLASS, arg, false);
		syn.setClassSeal(seal);
		return seal;
	}

	@Override
	public Seal visit(AsyncDecl syn, Seal container) throws FisherException {
		// TODO Auto-generated method stub
		return new Seal(syn, syn.name.str(), ASYNC, container, false);
	}

	@Override
	public Seal visit(SyncDecl syn, Seal container) throws FisherException {
		// TODO Auto-generated method stub
		return new Seal(syn, syn.name.str(), SYNC, container, false);
	}

	@Override
	public Seal visit(Spawn syn, Seal arg) throws FisherException {
		return new Seal(syn, syn.name.str(), SPAWN, arg, false);
	}

	@Override
	public Seal visit(JavalyFun syn, Seal arg) throws FisherException {
		return new Seal(syn, syn.name.str(), JAVALY_FUN, arg, false);
	}

	@Override
	public Seal visit(JavalyClassDecl syn, Seal arg) throws FisherException {
		Seal seal = new Seal(syn, syn.name.str(), JAVALY_CLS, arg, false);
		syn.name.setSeal(seal, syn);
		return seal;
	}

}
