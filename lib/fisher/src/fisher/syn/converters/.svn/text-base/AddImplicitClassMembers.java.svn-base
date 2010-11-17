
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.syn.converters;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.tree.ExpandVetoException;

import fisher.desugar.DesugarUtils;
import fisher.desugar.QueryDesugarer;
import fisher.parser.Token;
import fisher.statics.PredefinedIdentifiers;
import fisher.syn.*;
import fisher.syn.core.Id;
import fisher.syn.interfaces.ClassMember;
import fisher.syn.queries.Arity;
import fisher.syn.queries.FreeVariableNames;
import fisher.syn.queries.PatVars_Defined_By_Pattern;
import fisher.syn.queries.StringsDefinedByFormals;
import fisher.test.TestUtils;
import fisher.util.Bard;
import fisher.util.DangerLevel;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  AddImplicitClassMembers  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public static void to(List<ClassMember> members, Token start, Token end, Id name, boolean hasParams, List<ClassFormal> params,
			List<ClsExtends> ext, boolean isPure) {

		if (TestUtils.NO_CLASS_DESUGARING_SO_I_CAN_DO_PARSER_TESTS) {
			return;
		}

		try {
//			System.err.println("Add Implicit (START): name = " + name + "; params = "+ params);
//			System.err.println("Add Implicit (START): members = " + Bard.sep(members, "\n"));
			generateImplicitCtorAndFieldsIfNecessary(members, start, end, name, hasParams, params, ext, isPure);
			generateImplicitGetters(members, start, end, name);
			generateImplicitSetters(members, start, end, name);
			generateImplicitEqualityIfNeeded(members, start, end, name, isPure);
//			System.err.println("Add Implicit (END): members = " + Bard.sep(members, "\n"));
			
		} catch (FisherException fe) {
			throw new RuntimeException(fe);
		}
		// 
		//System.out.println("\nFinally-AICM, members = \n" + Bard.sep(members, "\n" ) );
		reset();
	}
	
	
	private static void generateImplicitEqualityIfNeeded(List<ClassMember> members, Token start, Token end,
			Id name, boolean isPure) throws FisherException{
		if (!isPure) return; // only pure classes get == by default.
		if (definesEqeq(members)) return; // Provides own definition of ==
		Id other = DesugarUtils.gensym(start, "other");
		Id other2 = other.internalDeepCopy(start, end);
		final Id same_type_etc = new Id(start, PredefinedIdentifiers.SAME_TYPE_AND_ALL_FIELDS_EQUAL);
		final VarExp same_type_pred = new VarExp(start, start, same_type_etc);
		final Cmd otherExp = (Cmd) new VarExp(start, start, other2);
		Cmd thisExp = new This(start,start);
		Cmd body = new FunCall(start, end, same_type_pred, Bard.list(thisExp, otherExp));
		MonoBody eqeqMonobod = new MonoBody(start, end, new Id(start, "=="),
				new Formals(start, end, Bard.list((Pat)new PatVar(start, start, other))),
				null, // from
				null, //envelope
				false, 0,
				body,
				false, // checked
				true // isMarkedPure
				);
		FunBody eqeqBod = new FunBody(start, end, Bard.list(eqeqMonobod), false);
		MethDecl eqeqMeth = new MethDecl(start, end, new Id(start, "=="), eqeqBod, true);
		eqeqMeth.allGenerated();
		members.add(eqeqMeth);
	}
	
	private static boolean definesEqeq(List<ClassMember> members){
		for (ClassMember m : members) {
			if (m instanceof MethDecl) {
				MethDecl mm = (MethDecl) m;
				if (mm.name.str().equals("==")) {
					if (Arity.hasArity(1, mm.funbody)) {
						return true;
					}
				}
			}
		}
		return false;
	}
	
	private static void generateImplicitGetters(List<ClassMember> members, Token start, Token end,
			Id name) throws FisherException{
		Set<String> fieldNames = allFieldNames(members);
		Set<String> getterLikeMethodNames = allGetterLikeMethodNames(members);
		Set<String> fieldsThatNeedImplicitGetters = Bard.minus(fieldNames, getterLikeMethodNames);
		for (String fieldName : fieldsThatNeedImplicitGetters) {
			// Generate: def $fieldName() = $fieldName;
			VarExp SfieldName = new VarExp(start, end, new Id(start, fieldName));
			MonoBody monob = new MonoBody(start, end,new Id(start, fieldName), new Formals(start, end, Collections.EMPTY_LIST),
					null, null, false, 0, SfieldName, false, false
			);
			FunBody funbody = new FunBody(start, end, Bard.list(monob), false);
			MethDecl meth = new MethDecl(start, end, new Id(start, fieldName), funbody, false);
			meth.allGenerated();
			members.add(meth);
		}
	}
	
	
	
	private static Set<String> allGetterLikeMethodNames(List<ClassMember> members) throws FisherException{
		Set<String> S = Bard.set();
		for (ClassMember cm : members) {
			if (cm instanceof MethDecl) {
				MethDecl md = (MethDecl) cm;
				FunBody fb = md.funbody;
				for (MonoBody monob : fb.funbodies) {
					if (monob.formals.formals.isEmpty()) {
						S.add(md.idOfName().str());
					}
				}
			}
		}
//		System.err.println("AddImplicitClassMembers: allGetterLikeMethodNames(" + Bard.sep(members, " ") + ") = {" + Bard.sep(S, ",")  + "}");
		return S;
	}
	
	private static Set<String> allFieldNames(List<ClassMember> members) throws FisherException{
		Set<String> S = Bard.set();
		for (ClassMember cm : members) {
			if (cm instanceof Bind) {
				Bind bind = (Bind) cm;
				final List<PatVar> patvars = PatVars_Defined_By_Pattern.of(bind.pat);
				for (PatVar patVar : patvars) {
					String s = patVar.id.str();
					S.add(s);
				}
			}
			else if (cm instanceof VarDecl) {
				VarDecl vd = (VarDecl) cm;
				String s = vd.var.str();
				S.add(s);
			}
		}
//		System.err.println("AddImplicitClassMembers: allFieldNames(" + Bard.sep(members, " ") + ") = {" + Bard.sep(S, ",")  + "}");
		return S;
	}

	
	
	private static void generateImplicitSetters(List<ClassMember> members, Token start, Token end,
			Id name) throws FisherException{
		Set<String> varNames = allVarNames(members);
		Set<String> varsWithSetterMethods = setterMethodNamesSansColonEq(members);
		Set<String> varsThatNeedSetters = Bard.minus(varNames, varsWithSetterMethods);
		for (String v : varsThatNeedSetters) {
			// def `v:=`(g) { v := g; }
			String setterName = v + ":=";
			final Id g = DesugarUtils.gensym(start, v);
			Id g2 = (Id) g.ingestedDeepCopy(start, end);
			List<AssignTarget> lhs = Bard.list((AssignTarget)new AssignToId(start, end, new Id(start, v)));
			List<Cmd> rhs = Bard.list((Cmd)(new VarExp(start, end, g)));
			Assign ass = new Assign(start, end, lhs, rhs);
			MonoBody monob = new MonoBody(start, end,new Id(start, setterName), 
					new Formals(start, end, Bard.list((Pat)new PatVar(start, end, g2))),
					null, null, false, 0, ass, false, false
			);
			FunBody funbody = new FunBody(start, end, Bard.list(monob), false);
			MethDecl meth = new MethDecl(start, end, new Id(start, setterName), funbody, false);
			meth.allGenerated();
			members.add(meth);
			
		}
		
	}
	
	private static Set<String> allVarNames(List<ClassMember> members) throws FisherException{
		Set<String> S = Bard.set();
		for (ClassMember cm : members) {
			if (cm instanceof VarDecl) {
				VarDecl vd = (VarDecl) cm;
				String s = vd.var.str();
				S.add(s);
			}
		}
//		System.err.println("AddImplicitClassMembers: allVarNames(" + Bard.sep(members, " ") + ") = {" + Bard.sep(S, ",")  + "}");
		return S;
	}

	private static Set<String> setterMethodNamesSansColonEq(List<ClassMember> members) throws FisherException{
		Set<String> S = Bard.set();
		for (ClassMember cm : members) {
			if (cm instanceof MethDecl) {
				MethDecl md = (MethDecl) cm;
				final String mdName = md.idOfName().str();
				if (! (mdName.endsWith(":="))) continue;
				FunBody fb = md.funbody;
				for (MonoBody monob : fb.funbodies) {
					if (monob.formals.formals.size() == 1) {
						S.add(mdName.substring(0, mdName.length()-2));
					}
				}
			}
		}
//		System.err.println("AddImplicitClassMembers: setterMethodNamesSansColonEq(" + Bard.sep(members, " ") + ") = {" + Bard.sep(S, ",")  + "}");
		return S;
	}

	private static void generateImplicitCtorAndFieldsIfNecessary(List<ClassMember> members, Token start, Token end,
			Id name, boolean hasParams,List<ClassFormal> params, List<ClsExtends> ext, boolean isPure) throws FisherException {
		if (hasParams) {
			reset();
			Set<String> fieldNamesFromFormals = fieldNamesFromFormals(params);
			Set<String> fieldNamesForwardedToSupers = FreeVariableNames.of(ext);
			Set<String> fieldNamesToGen = Bard.minus(fieldNamesFromFormals, fieldNamesForwardedToSupers);
			genFieldsFromParameters(members, start, fieldNamesToGen, params);
			genImplicitCtor(members, start, end, name, params, ext, isPure, fieldNamesToGen);
			makeSureForwardedFieldsArePlainIds(params, fieldNamesForwardedToSupers);
		}
	}
	
	private static void makeSureForwardedFieldsArePlainIds(List<ClassFormal> params, Set<String> fieldNamesForwardedToSupers) {
		for (ClassFormal cr : params) {
			if (fieldNamesForwardedToSupers.contains(cr.name.str()) ){
				if (cr.isVar) {
					cr.flag(DangerLevel.ERROR, "Forwarded class parameters cannot be 'var'", "", "like: " + cr);
				}
				if (cr.typeConstraints.constraints.size() > 0) {
					cr.flag(DangerLevel.ERROR, "Forwarded class parameters cannot have type constraints", "", "like: " + cr);
				}
			}
		}
	}
	
	private static Set<String> fieldNamesFromFormals(List<ClassFormal> params) {
		Set<String> S = new HashSet<String>(params.size());
		for (ClassFormal cf : params) {
			S.add(cf.name.str());
		}
		return S;
	}

	private static void genImplicitCtor(List<ClassMember> members, Token start, Token end, Id name, List<ClassFormal> params,
			List<ClsExtends> ext, boolean isPure, Set<String> fieldNamesToGen) {
		Id clsId = (Id) name.ingestedDeepCopy(start, start);
		MonoBody monobody = new MonoBody(start, start, (Id) name.ingestedDeepCopy(start, end), genCtorFormals(params,
				start), null, null, false, 0, // Distribution stuff -- not here.
				genCtorBody(params,start, fieldNamesToGen, ext), false, isPure);
		ClsCtorDef implicitCtor = new ClsCtorDef(start, end, clsId, monobody);
		implicitCtor.allGenerated();
		members.add(implicitCtor);
	}

	private static void genFieldsFromParameters(List<ClassMember> members, Token start, Set<String> fieldNamesToGen, List<ClassFormal> params) {
		// Generate fields from parameters.
//		for (String gen : fieldNamesToGen) {
//			// Generate: val $gen
//			Id genId = new Id(start, gen);
//			Pat genPat = new PatVar(start, start, genId);
//			Bind bind = new Bind(start, start, genPat, null);
//			members.add(bind);
//		}
		for (ClassFormal classFormal : params) {
			final String name = classFormal.name.str();
			if (fieldNamesToGen.contains(name)) {
				if (classFormal.isVar) {
					// gen a var
					Id var = new Id(start, name);
					VarDecl varDecl = new VarDecl(start, start, var, null, classFormal.typeConstraints);
					varDecl.allGenerated();
					members.add(varDecl);
				}
				else  {
					// gen a bind
					Id genId = new Id(start, name);
					Pat genPat = new PatVar(start, start, genId);
					final List<TypeConstraint> constraints = classFormal.typeConstraints.constraints;
					Bind bind = new Bind(start, start, genPat, null);
					bind.allGenerated();
					members.add(bind);
				}
			}
		}
	}

	private static Map<String, Id> cacheId = new HashMap<String, Id>();

	private static void reset() {
		cacheId.clear();
	}

	private static Id formalFor(String fieldName, Token start) {
		if (!cacheId.containsKey(fieldName)) {
			Id ff = DesugarUtils.gensym(start, fieldName);
			cacheId.put(fieldName, ff);
		}
		return cacheId.get(fieldName);
	}

	private static Cmd genCtorBody(List<ClassFormal> params, Token start, Set<String> fieldNamesToGen, List<ClsExtends> ext) {
		List<Cmd> L = new ArrayList<Cmd>();
		// new@A(x,y) calls for each superclass A(x,y)
		for (ClsExtends ce : ext) {
			SuperCtorCall newcall = new SuperCtorCall(start, start, (QualName) ce.superName.ingestedDeepCopy(start,
					start), DesugarUtils.deepCopyCmdsRenaming(start, start, ce.args, cacheId));
			L.add(newcall);
		}

		// x = x' or x := x' for each non-forwarded var from the formals.
		for (ClassFormal cf : params) {
			String fn = cf.name.str();
			if (fieldNamesToGen.contains(fn)) {
				if(cf.isVar) {
					Id formally = (Id) formalFor(fn, start).ingestedDeepCopy(start, start);
					Id fieldly = new Id(start, fn);
					AssignToId lhs = new AssignToId(start, start, fieldly);
					Assign assign = new Assign(start, start, Bard.list((AssignTarget)lhs), Bard.list((Cmd)new VarExp(start,start, formally)));
					L.add(assign);
				}
				else {
					Id formally = (Id) formalFor(fn, start).ingestedDeepCopy(start, start);
					Id fieldly = new Id(start, fn);
					PatVar fieldPV = new PatVar(start, start, fieldly);
					Cmd initialValue = new VarExp(start, start, formally);
					for (TypeConstraint tc : cf.typeConstraints.constraints) {
						initialValue =new TypedExp(start, start, initialValue, new QualName(start, start, Bard.list(tc.typename)));
					}
					Bind bind = new Bind(start, start, fieldPV, initialValue);
					L.add(bind);
				}
			}
		}

//		for (String fn : fieldNamesToGen) {
//			Id formally = (Id) formalFor(fn, start).ingestedDeepCopy(start, start);
//			Id fieldly = new Id(start, fn);
//			PatVar fieldPV = new PatVar(start, start, fieldly);
//			Bind bind = new Bind(start, start, fieldPV, new VarExp(start, start, formally));
//			L.add(bind);
//		}

		Seq seq = new Seq(start, start, L);
		return seq;
	}

	/**
	 * @param formals
	 * @param start
	 * @return Formals( [a', b'] ), if input was [a,b]
	 */
	private static Formals genCtorFormals(List<ClassFormal> params, Token start) {
		if (params == null)
			return null;
		
		List<Pat> ctorFormals = new ArrayList<Pat>();
		for (ClassFormal cf : params) {
			Id i0 = cf.name;
			Id f0 = (Id) formalFor(i0.str(), start).ingestedDeepCopy(start, start);
			ctorFormals.add(new PatVar(start, start, f0));
		}
		//OLD...
//		for (Pat p : formals.formals) {
//			if (p instanceof PatVar) {
//				PatVar pv = (PatVar) p;
//				Id i0 = pv.id;
//				Id f0 = (Id) formalFor(i0.str(), start).ingestedDeepCopy(start, start);
//				ctorFormals.add(new PatVar(start, start, f0));
//			} else {
//				Doom.internalCatastrophe("Can't deal with class formal that isn't a simple variable", formals);
//			}
//		}
		return new Formals(start, start, ctorFormals);
	}
}
