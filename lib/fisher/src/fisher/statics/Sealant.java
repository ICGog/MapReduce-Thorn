
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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import fisher.runtime.auxil.ColInfo;
import fisher.syn.*;
import fisher.syn.core.ColAccess;
import fisher.syn.core.ColSpecial;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.syn.importage.Important;
import fisher.syn.interfaces.ClassMember;
import fisher.syn.interfaces.Classlike;
import fisher.syn.interfaces.ISyntax;
import fisher.syn.interfaces.TableMember;
import fisher.syn.visitor.VanillaVisitor;
import fisher.util.Bard;
import fisher.util.Compilation;
import fisher.util.DangerLevel;
import fisher.util.Doom;
import fisher.util.Fisher;
import fisher.util.FisherException;
import fisher.util.FisherInternalCompilerDoom;

public  class  Sealant extends VanillaVisitor<Env, Env, FisherException>  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public static boolean finalCheck(Syntax... syns) {
		boolean good = true;
		for (Syntax syn : syns) {
			for (Id id : allIdsIn(syn)) {
				if (id.seal() == null) {
					id.flag(DangerLevel.ERROR, "Undefined identifier: " + id, "");
					good = false;
				}
			}
		}
		return good;
	}

	public static List<Id> allIdsIn(Syntax syn) {
		List<Syntax> allDesc = syn.computeDescendants();
		List<Id> ids = new ArrayList<Id>();
		for (Syntax desc : allDesc) {
			if (desc instanceof Id) {
				Id id = (Id) desc;
				ids.add(id);
			}
		}
		return ids;
	}

	public <T> Env sealInOrder(List<T> syns, Env env) throws FisherException {
		for (Object syn : syns) {
			env = ((Syntax) syn).accept(this, env);
		}
		return env;
	}

	public void sealIndependently(List<? extends Syntax> syns, Env env) throws FisherException {
		for (Syntax syntax : syns) {
			Env inner = env.inner();
			syntax.accept(this, inner);
		}
	}

	public void sealIndependently(Syntax syn, Env env) throws FisherException {
		if (syn != null)
			syn.accept(this, env.inner());
	}

	public void produceTrashSealsForDeadCode(Syntax syn) throws FisherException {
		List<Syntax> descendants = syn.computeDescendants();
		for (Syntax syntax : descendants) {
			if (syntax instanceof Id) {
				Id id = (Id) syntax;
				id.setSeal(SealMaker.ofIdInDeadCode(id.str()), id);
			}
		}
	}

	public Env sealSeq(Env env, List<? extends ISyntax> cmds, boolean returnInner) throws FisherException {
		// First pass, to get seals for circular defs.
		Env innerEnv = env.inner();
		// Carefully propagate 'new'-ness, from the monobody of 'new C(x){v=x;}'
		// to the 'v=x;' part.
		innerEnv.setDoingNew(env.doingNew());
		SealCircularDefs.seal(cmds, innerEnv);
		// Second pass, to get all the details
		sealInOrder(cmds, innerEnv);
		return returnInner ? innerEnv : env;
	}

	private Env sealMethodlyThing(Env env, Id name, FunBody funbody) throws FisherException {
		boolean b = name.seal() != null;
		if (!b) {
			// set a finger-embroidering breakpoint here, sheesh.
			assert (b);
		}

		Seal seal = name.seal();
		// Make sure the monobodies are all sealed, not that that is used for anything but testing.
		for (MonoBody mb : funbody.funbodies) {
			if (mb.id.seal() == null)
				mb.id.setSeal(seal, mb);
		}

		// env.define(name.str(), name.seal());

		sealThus(env.innerWithNewContainer(name.seal()), funbody);
		return env;
	}

	// public Env sealThus(Env env, Syntax ... subs) throws FisherException {
	// for (Syntax syntax : subs) {
	// env = syntax.accept(this, env);
	// }
	// return env;
	// }

	public Env sealThus(Env env, Object... stuff) throws FisherException {
		for (Object object : stuff) {
			if (object == null) {
				// Useful for optional bits, like in 'var x := e'
				continue;
			}
			if (object instanceof Syntax) {
				Syntax syntax = (Syntax) object;
				env = syntax.accept(this, env);
			} else if (object instanceof List) {
				List Lsyn = (List) object;
				for (Object o : Lsyn) {
					env = sealThus(env, o);
				}
			} else {
				Doom.internal("Unknown thing in 'sealThus'", null, stuff, stuff.getClass());
			}
		}
		return env;
	}

	/**
	 * @param qn
	 * @param env
	 * @return the seal of the final member of qn. Side effect: set seals of all
	 *         members of qn.
	 * @throws FisherException
	 */
	public Seal sealQualName(QualName qn, Env env) throws FisherException {
		final int nNames = qn.ids.size();
		if (nNames <= 0)
			qn.flag(DangerLevel.ERROR, "No names in qualname?", "There ought to be some names here");
		ModuleStatic ms = null;
		for (int i = 0; i < nNames - 1; i++) {
			Id id = qn.ids.get(i);
			Seal seal;
			if (ms == null) {
				seal = env.seal(id, qn);
			} else {
				seal = ms.findSealForMember(id.str(), qn);
			}
			id.setSeal(seal, qn);
			Seal origseal = seal.dealias();
			if (origseal instanceof SealForModule) {
				SealForModule s4mod = (SealForModule) origseal;
				ms = s4mod.moduleStatic;
			} else {
				id.flag(DangerLevel.ERROR, "Not a module: " + id,
						"This context requires that you qualify a module name");
			}

		}

		Id id = qn.ids.get(nNames - 1);
		Seal s = ms == null ? env.seal(id, qn) : ms.findSealForMember(id.str(), qn);
		id.setSeal(s, qn);
		return s;
	}

	private static void checkForShadowing(Syntax syn, Env arg, String varname) {
		// I think this is obsolete in both places it is used.
		//if (arg.hasSealFor(varname)) {
		//	syn.flag(DangerLevel.ERROR, "Illegal shadow binding for " + varname + ".", "");
		//}
	}

	@Override
	public Env visit(Syntax syn, Env arg) throws FisherException {
		Doom.internalCatastrophe("The doom! Sealant doesn't know how to figure out seals for a " + syn.getClass(), syn);
		return arg;
	}

	private static void setSeal(Id id, Seal seal) throws FisherException {
		id.setSeal(seal, id);
	}

	@Override
	public Env visit(VarExp syn, Env env) throws FisherException {
		Id id = syn.id;
		Seal seal = env.seal(id, syn);
		setSeal(id, seal);
		return env;
	}

	@Override
	public Env visit(OpExp syn, Env env) throws FisherException {
		if (syn.op.shouldBeSealedIndependently()) {
			sealIndependently(syn.operands, env);
			return env;
		} else {
			return sealThus(env, syn.operands);
		}
	}

	@Override
	public Env visit(Comparison syn, Env env) throws FisherException {
		return sealThus(env, syn.first, syn.rest);
	}

	@Override
	public Env visit(ComparisonBit syn, Env env) throws FisherException {
		return sealThus(env, syn.to);
	}

	@Override
	public Env visit(Seq syn, Env env) throws FisherException {
		List<Cmd> cmds = syn.cmds;
		return sealSeq(env, cmds, false);
	}

	@Override
	public Env visit(Signature syn, Env env) throws FisherException {
		Env loopEnv = sealLoopLabel(syn, env);
		final Cmd body = syn.body;
		int hashCode = body.hashCode();
		if (hashCode != syn.signature) {
			syn.flag(DangerLevel.ERROR, "Signatures do not match: actual=" + hashCode + "; desired=" + syn.signature, "");
		}
		sealThus(loopEnv, body);
		return env;
	}
	
	@Override
	public Env visit(Bind syn, Env env) throws FisherException {
		// Odd special case [1]: pat p(x) { x = 1; }
		// Odd special case [2]: class C{ val v; new C(x) { v = x; }

		if (syn.pat instanceof PatVar) {
			PatVar pv = (PatVar) syn.pat;
			if (env.hasSealFor(pv.id.str())) {
				Seal pvSeal = env.seal(pv.id.str(), syn);
				if (pvSeal.kind == SealKind.PAT_FORMAL) {
					// OK, it is in the special case [1]!
					pv.id.setSeal(pvSeal, syn);
					syn.reallyAnAssignmentToAPatVariable = true;
					return sealThus(env, syn.exp);
				}// end of special case [1]
				else if (env.doingNew() && pvSeal instanceof SealForVal && ((SealForVal) pvSeal).needsInit) {
					// OK, it is in special case [2]
					pv.id.setSeal(pvSeal, syn);
					syn.reallyAnInitializingAssignmentToAValField = true;
					return sealThus(env, syn.exp);
				}
			}
		}

		// // Odd special case [3]: class C{ val v; } -- the 'val v;' is counted
		// as a bind.
		// That special case has been taken care of in ExtractSealsFromClsDecl,
		// hopefully.
		// if (env.doingClass() && syn.exp == null) {
		// if (syn.pat instanceof PatVar) {
		// PatVar pv = (PatVar) syn.pat;
		// SealForVal seal = SealMaker.ofUninitVal(pv.id, syn, env.container);
		// }
		// else {
		// syn.flag(DangerLevel.ERROR, "Incomplete val declarations have to
		// declare single variables.", "They should look like 'val v;'");
		// }
		// }

		return sealThus(env, syn.exp, syn.pat);
	}

	@Override
	public Env visit(Literal syn, Env arg) throws FisherException {
		return arg;
	}

	@Override
	public Env visit(PatVar syn, Env env) throws FisherException {
		String varname = syn.id.str();
		checkForShadowing(syn, env, varname);
		Seal seal = SealMaker.of(syn, env.container);
		env.define(varname, seal);
		syn.id.setSeal(seal, syn);
		return env;
	}

	@Override
	public Env visit(PatLiteral syn, Env env) throws FisherException {
		return env;
	}

	@Override
	public Env visit(PatListCtor syn, Env arg) throws FisherException {
		return sealThus(arg, syn.bits);
	}

	@Override
	public Env visit(PatListBitEllip syn, Env arg) throws FisherException {
		return sealThus(arg, syn.pat);
	}

	@Override
	public Env visit(PatListBitExp syn, Env arg) throws FisherException {
		return sealThus(arg, syn.pat);
	}

	@Override
	public Env visit(PatTypeTest syn, Env env) throws FisherException {
		sealQualName(syn.shouldBe, env);
		return sealThus(env, syn.subpat);
	}

	@Override
	public Env visit(TypedExp syn, Env env) throws FisherException {
		sealQualName(syn.type, env);
		return sealThus(env, syn.exp);
	}

	@Override
	public Env visit(PatSlash syn, Env arg) throws FisherException {
		return sealThus(arg, syn.actualCode);
	}

	public Env visit(PatRange syn, Env env) throws FisherException {
		return sealThus(env, syn.low, syn.high);
	}

	@Override
	public Env visit(VarDecl syn, Env env) throws FisherException {
		env = sealThus(env, syn.init, syn.typeConstraints);
		final Id var = syn.var;
		String varname = var.str();
		return sealSimple(syn, env, var, varname);
	}

	private Env sealSimple(Cmd syn, Env env, final Id var, String varname) throws FisherException {
		checkForShadowing(syn, env, varname);
		Seal seal = SealMaker.of(syn, env.container);
		env.define(varname, seal);
		var.setSeal(seal, syn);
		return env;
	}

	@Override
	public Env visit(TypeConstraints syn, Env arg) throws FisherException {
		return sealThus(arg, syn.constraints);
	}

	@Override
	public Env visit(TypeConstraint syn, Env env) throws FisherException {
		Seal seal = env.seal(syn.typename, syn);
		setSeal(syn.typename, seal);
		return env;
	}

	@Override
	public Env visit(Assign syn, Env env) throws FisherException {
		sealThus(env, syn.lhs, syn.rhs);
		for (AssignTarget targ : syn.lhs) {
			if (targ instanceof AssignToId) {
				AssignToId a2i = (AssignToId) targ;
				Id id = a2i.id;
				if (id.seal() == null) {
					syn.flag(DangerLevel.ERROR, "Undeclared variable", "");
				} else if (id.seal().kind != SealKind.VAR) {
					syn.flag(DangerLevel.ERROR, "Assignment to non-variable",
							"The only identifiers which can be :='ed to are those which have been declared 'var'.  "
									+ id + " is " + id.seal().kind);
				}
			}
		}
		return env;
	}

	@Override
	public Env visit(ListCtor syn, Env env) throws FisherException {
		return sealThus(env, syn.bits);
	}

	@Override
	public Env visit(ListBitExp syn, Env arg) throws FisherException {
		return sealThus(arg, syn.exp);
	}

	@Override
	public Env visit(ListBitEllip syn, Env arg) throws FisherException {
		return sealThus(arg, syn.exp);
	}

	@Override
	public Env visit(AssignToId syn, Env env) throws FisherException {
		String varname = syn.id.str();
		// DO NOT checkForShadowing(syn, env, varname);! This confused me for a
		// bit, but it's ok for a to be defined in a:=b;
		Seal seal = env.seal(varname, syn);
		if (seal != null) {
			// HUH env.define(seal);
			syn.id.setSeal(seal, syn);
		}
		return env;
	}

	@Override
	public Env visit(AssignToSubscripted syn, Env env) throws FisherException {
		sealIndependently(syn.subscripts, env);
		sealThus(env, syn.array);
		return env;
	}

	@Override
	public Env visit(AssignToMap syn, Env env) throws FisherException {
		sealIndependently(syn.subscripts, env);
		sealThus(env, syn.map);
		return env;
	}

	@Override
	public Env visit(AssignToFieldOfSubscripted syn, Env env) throws FisherException {
		sealIndependently(syn.subscripts, env);
		sealThus(env, syn.array);
		sealFieldRefLikeThing(syn, env, null, syn.field);
		return env;
	}

	@Override
	public Env visit(Probe syn, Env env) throws FisherException {
		{
			Seal probeseal = SealMaker.of(syn, env.container);
			if (syn.id != null)
				syn.id.setSeal(probeseal, syn);
			// But that seal doesn't go into the environment
		}
		// Different kinds of probes can have different sealing behavior, so
		// defer to the prober.
		if (syn.prober == null) {
			syn.flag(DangerLevel.ERROR, "unknown kind of prober: " + syn.id, "");
		} else {
			env = syn.prober.seal(syn, env, this);
		}
		return env;
	}

	@Override
	public Env visit(ImportStmt syn, Env env) throws FisherException {
		for (Important important : Important.values()) {
			Env e2 = important.trySealing(syn, env);
			if (e2 != null)
				return e2;
		}
		syn.flag(DangerLevel.ERROR, "This import statement is not one of the approved forms of import statement. \n",
				"The approved forms are \n" + Important.approvedForms(), syn, env);
		return env;
	}

	@Override
	public Env visit(AnonFun syn, Env arg) throws FisherException {
		sealThus(arg, syn.fun);
		return arg; // No bindings from inside get produced outside.
	}

	@Override
	public Env visit(FunDecl syn, Env env) throws FisherException {
		// syn.id is sealed by the first pass, using SealCircularDefs.
		assert (syn.name.seal() != null);
		return sealThus(env, syn.funbody);
	}

	@Override
	public Env visit(FunBody syn, Env arg) throws FisherException {
		for (MonoBody monobody : syn.funbodies) {
			sealThus(arg, monobody);
		}
		return arg;// No bindings from inside get produced outside.
	}

	@Override
	public Env visit(MonoBody syn, Env env) throws FisherException {
		Env envForFormals = env.innerWithNoContainer();
		// Propagate, carefully, from the CtorDecl 'new C(x){v=x;}' to its
		// monobody.
		envForFormals.setDoingNew(env.doingNew());
		if (syn.id != null && syn.id.seal() == null) {
			Seal sealFromEnv = env.seal(syn.id, syn);
			if (sealFromEnv == null) {
				// We don't care about these seals very much. I hope.
				// This can be null in the case of a multi-clause method decl: 
				// due to the name/arity nature of method names, 
				// we can't store their seals in the environment.
				// So, just make up a seal.
				// Sorry if you actually needed that seal for something.
				// The "dead" here is just the method name.
				sealFromEnv = SealMaker.ofIdInDeadCode(syn.id.str());
			}
			syn.id.setSeal(sealFromEnv, syn);
		}
		sealThus(envForFormals, syn.formals, syn.from, syn.envelope, syn.body);
		return env; // No bindings from inside get produced outside.
	}

	@Override
	public Env visit(Formals syn, Env arg) throws FisherException {
		return sealThus(arg, syn.formals);
	}

	@Override
	public Env visit(FunCall syn, Env arg) throws FisherException {
		return sealThus(arg, syn.function, syn.args);
	}

	@Override
	public Env visit(Parens syn, Env arg) throws FisherException {
		return sealThus(arg, syn.exp);
	}

	@Override
	public Env visit(ModulesInAList syn, Env env) throws FisherException {
		for (Module syntax : syn.modules) {
			SealForModule container = syntax.moduleSeal;
			Env inner = env.innerWithNewContainer(container);
			syntax.accept(this, inner);
		}
		return env;
	}

	@Override
	public Env visit(ModuleFileVisibility syn, Env env) throws FisherException {
		Id id = syn.id;
		Seal seal = env.seal(id, syn);
		setSeal(id, seal);
		return env;
	}

	@Override
	public Env visit(Module syn, Env env) throws FisherException {
		if (syn.name.ids.size() != 1) {
			Doom.notYet("We don't yet handle module A.B{}");
		}

		Id moduleId = syn.name.last();
		ModuleStatic ms = env.findAvailableModule(moduleId, syn);
		if (ms == null) {
			Doom.internal("Please use an ExtractSealsFromModule on this module before applying sealant", syn, env);
		}
		SealForModule moduleSeal = ms.moduleSeal();
		moduleId.setSeal(moduleSeal, syn);

		// And now everything else, which is approximately trivial, and
		// basically like a Seq
		env = sealSeq(env, syn.bits, false);
		return env;
	}

	@Override
	public Env visit(ClsDecl syn, Env env) throws FisherException {
		return sealizeClasslike(syn, env);
	}

	@Override
	public Env visit(AnonObj syn, Env env) throws FisherException {
		// EX: return sealizeClasslike(syn, env);
		return sealThus(env, syn.actualCode);
	}

	private Env sealizeClasslike(Classlike syn, Env env) throws FisherException {
		Id name = syn.name(); // may be null, for anon obj.
		//Formals formals = syn.formals(); // may be null, for anon obj,
		List<ClassFormal> params = syn.params();
		List<ClassMember> members = syn.members();
		List<ClsExtends> exts = syn.extendses();

		Syntax synsyn = (Syntax) syn;
		SealForClass seal = (SealForClass) SealMaker.of(synsyn, env.container);

		if (name != null) {
			env.define(name.str(), seal);
			name.setSeal(seal, synsyn);
		}

		Env classEnv = env.innerWithNewContainer(seal);

		Env teenyEnvForSuperclassCalls = classEnv.inner();
		// formals
		if (params != null) {
			sealThus(teenyEnvForSuperclassCalls, params);
		}
		//

		sealThus(teenyEnvForSuperclassCalls, exts);
		// 

		// We need to do a somewhat delicate two-phase process here.
		/*
		 * Consider a class with mutually recursive methods: class C { meth f(n) {
		 * ... this.g(n-1) ... } meth g(n) { ... this.f(n-1) ... } }C We need to
		 * get seals for all the methods and stuff in phase 1, and then seal the
		 * bodies in phase 2. This is much like for modules. However, for
		 * modules we do the two in two separate places (cf.
		 * ExtractSealsFromModuleMember) (because modules can import each
		 * other), and for classes we do it all here.
		 */
		// Phase 1
		getSealsForTopLevelClassMembers(syn, members, classEnv);

		// Oh, and we want the ClassStatic too
		ClassStatic cs = new ClassStatic(syn, env);
		syn.setClassStatic(cs);
		seal.setClassStatic(cs);
		classEnv.setClassStatic(cs);

		// Also, superclasses may define vars which are used in syn:
		// class A{a=1;}
		// class B extends A {b = a+1;}
		// ACTUALLY, the team decided to reverse this decision
		// so now 'a' is out of scope.
		// So, this line gets commented out: 
		//WAS: 
		//HERE-PRIV
		if (!Fisher.FIELDS_ARE_PRIVATE)  addFieldSealsOfSuperclasses(syn, cs, classEnv);

		// Phase 2
		sealThus(classEnv, members);

		// And record the seals of the members in cs.
		final Set<String> fieldNamesToAddToFieldSeals = 
			Fisher.FIELDS_ARE_PRIVATE ? cs.namesOfFieldsDefinedByThisClassButNotParents() : cs.fieldNames;
		for (String s : fieldNamesToAddToFieldSeals) {
			Seal s2 = classEnv.seal(s, synsyn);
			cs.fieldSeals.add(s2);
			//			cs.fieldName2Seal.put(s, s2);
			if (syn.isMarkedPure() && s2.kind == SealKind.VAR) {
				((Syntax) syn).flag(DangerLevel.ERROR, "A pure class cannot have a var field, like " + s, "");
			}
		}

		// Phase 3 -- other static checks.

		checkSuperclassesForUnresolvedConflicts(syn, cs);

		// Finally
		return env;
	}

	private void checkSuperclassesForUnresolvedConflicts(Classlike src, ClassStatic cs) throws FisherException {
		Set<MethodSig> sigsHere = cs.sigsOfMethodsDefinedLocally();
		// %group(sig = sig){parent = %list p | for {: parent: p, sig :} <- something() };
		Map<MethodSig, List<ClassStatic>> M = new HashMap<MethodSig, List<ClassStatic>>();
		for (ClassStatic parent : cs.superclasses) {
			Set<MethodSig> sigsOfParent = parent.sigsOfApplicableMethods();
			for (MethodSig methodSig : sigsOfParent) {
				if (M.containsKey(methodSig)) {
					final List<ClassStatic> L = M.get(methodSig);
					ClassStatic previouslyDefiningParent = L.get(0);
					final MethDecl methOrNull = parent.findMethodOrNull(methodSig);
					assert (methOrNull != null);
					assert (methOrNull.origin != null);
					final MethDecl otherMethod = previouslyDefiningParent.findMethodOrNull(methodSig);
					if (otherMethod.origin == methOrNull.origin) {
						// The two definitions are the same ast node.
						// Which the two parents get the same def from a common parent...
						// They don't define it themselves.
						continue;
					}
					L.add(parent);
				} else {
					M.put(methodSig, Bard.list(parent));
				}
			}
		}
		for (Map.Entry<MethodSig, List<ClassStatic>> sigParents : M.entrySet()) {
			MethodSig ms = sigParents.getKey();
			List<ClassStatic> parents = sigParents.getValue();
			if (parents.size() > 1 && !sigsHere.contains(ms)) {
				// 2+ parents define it, but this class doesn't. So complain!
				((Syntax) src).flag(DangerLevel.ERROR, "Must override multiply-inherited method " + ms, "Superclasses "
						+ Bard.sep(parents, ", ") + " all define " + ms + " and so this class must define it as well.");
			}
		}

	}

	private void addFieldSealsOfSuperclasses(Classlike cl, ClassStatic cs, Env classEnv) throws FisherException {
		for (ClassStatic superClassStatic : cs.superclasses) {
			for (Seal seal : superClassStatic.fieldSeals) {
				if (SealKind.shouldBeAddedToEnvOfSubclass.contains(seal.kind)) {
					classEnv.define(seal.str(), seal);
				}
			}
		}
	}

	@Override
	public Env visit(ClsExtends syn, Env env) throws FisherException {
		if (!env.hasSealFor(syn.superName)) {
			syn.flag(DangerLevel.ERROR, "No definition for superclass " + syn.superName, "");
			return env;
		}
		Seal superseal = sealQualName(syn.superName, env);
		syn.superclassSeal = superseal;
		if (superseal.dealias() instanceof SealForClass) {
			// all is well.
		} else {
			syn.flag(DangerLevel.ERROR, "Not a class", syn.superName + " is not a class.  It's a " + superseal.kind
					+ ".");
		}
		sealThus(env, syn.args);
		return env;

	}

	private void getSealsForTopLevelClassMembers(Classlike clsDecl, List<ClassMember> members, Env classEnv)
			throws FisherException {
		ExtractSealsFromClsDecl extractor = new ExtractSealsFromClsDecl(clsDecl, classEnv, this);
		for (ClassMember classMember : members) {
			((Syntax) classMember).accept(extractor, "nothing useful goes here, but my code generator requires an arg");
		}
	}

	@Override
	public Env visit(ClsCtorDef syn, Env env) throws FisherException {
		// new c(x,y){Body(x,y);}
		// c is the same as the class name, and does not get a separate seal, I
		// guess.
		Id id = syn.id;
		if (id != null) {
			assert (id.str().equals(env.container.str()));
			id.setSeal(env.container, syn);
		}
		// The rest is just a monobody.
		sealThus(env.innerNew(), syn.monobody);
		// 
		return env;
	}

	@Override
	public Env visit(ClsPatDef syn, Env env) throws FisherException {
		// pat p(x,y) {x = 1; y=2; }
		// p
		Id id = syn.id;
		Seal seal = SealMaker.of(syn, env.container);
		id.setSeal(seal, syn);
		env.define(id.str(), seal);

		Env envInsidePat = env.innerWithNewContainer(seal);
		// x,y -- note that they are just Ids, but, being pat formals, they are
		// treated strangely.
		for (Id formal : syn.formals) {
			if (!(env.container instanceof SealForClass)) {
				Doom.internal("I dunno", syn);

			}
			Seal patSeal = SealMaker.ofPatFormal(formal, syn, (SealForClass) env.container);
			formal.setSeal(patSeal, syn);
			envInsidePat.define(formal.str(), patSeal);
		}
		// {x=1;y=2;}
		sealThus(envInsidePat, syn.body);
		return env;
	}

	@Override
	public Env visit(MethDecl syn, Env env) throws FisherException {
		Id name = syn.name;
		FunBody funbody = syn.funbody;

		return sealMethodlyThing(env, name, funbody);
	}

	@Override
	public Env visit(FieldRef syn, Env env) throws FisherException {
		Cmd target = syn.target;
		return sealFieldRefLikeThing(syn, env, target, syn.field);
	}

	private Env sealFieldRefLikeThing(Syntax syn, Env env, Cmd target, Id field) throws FisherException {
		if (target != null)
			env = sealThus(env, target);
		if (isModule(target, env)) {
			sealModuleField(target, field, env);
		} else {
			Seal fielSeal = SealMaker.of(syn, env.container);
			field.setSeal(fielSeal, syn);
			// Don't bother to put it in the env -- field accesses are unique, I
			// think.
		}
		return env;
	}

	private void sealModuleField(Cmd cmd, Id field, Env env) throws FisherException {
		// Code loosely parallels isModule
		if (cmd instanceof VarExp) {
			VarExp vexp = (VarExp) cmd;
			Id id = vexp.id;
			Seal s = id.seal();
			if (s == null) {
				Doom.internal("I thought it was sealed as a module, but it's unsealed...?", cmd);
				return;
			}
			Seal sorig = s;
			s = s.dealias();
			assert (s.kind == SealKind.MODULE);
			String fieldStr = field.str();
			for (Seal skid : s.contents) {
				if (skid.str().equals(fieldStr)) {
					field.setSeal(skid, cmd);
					return;
				}
			}
			field.flag(DangerLevel.ERROR, "Not a member of this module", "There is no member " + fieldStr
					+ " in module " + s);
		} else {
			Doom.internal("Trying to seal a module that is not a module", cmd);
			return;
		}
	}

	public static boolean isModule(Cmd cmd, Env env) {
		// Code loosely parallels sealModuleField
		if (cmd instanceof VarExp) {
			VarExp vexp = (VarExp) cmd;
			Id id = vexp.id;
			Seal s = id.seal();
			if (s == null) {
				// cmd isn't defined, so it kind of neither is nor is not a
				// module. Gotta return something.
				// Can't even throw an error. User is kinda in for a cascade of
				// errors anyways.
				return false;
			}
			return s.kind == SealKind.MODULE;
		} else {
			return false;
		}
	}

	@Override
	public Env visit(AssignTofield syn, Env env) throws FisherException {
		return sealFieldRefLikeThing(syn, env, syn.target, syn.field);
	}

	@Override
	public Env visit(MethodCall syn, Env env) throws FisherException {
		Seal methSeal = SealMaker.of(syn, env.container);
		syn.method.setSeal(methSeal, syn);
		return sealThus(env, syn.target, syn.args);
	}

	@Override
	public Env visit(This syn, Env arg) throws FisherException {
		return arg;
	}

	@Override
	public Env visit(Valof syn, Env env) throws FisherException {
		sealThus(env, syn.stmts);
		return env;
	}

	@Override
	public Env visit(For syn, Env env) throws FisherException {
		Env loopEnv = sealLoopLabel(syn, env);
		sealThus(loopEnv, syn.list, syn.pat, syn.body);
		return env;
	}

	@Override
	public Env visit(While syn, Env env) throws FisherException {
		Env loopEnv = sealLoopLabel(syn, env);

		if (syn.reallyUntil)
			return sealUntil(syn, env, loopEnv);
		if (syn.reallyDo && !syn.reallyUntil)
			return sealDoWhile(syn, env, loopEnv);
		if (!syn.reallyDo && !syn.reallyUntil)
			return sealWhile(syn, env, loopEnv);
		Doom.internal("math is broken", syn);
		return null;

		// sealThus(inner, syn.test, syn.body);
		// return env;
	}

	private Env sealLoopLabel(LabellableLoop syn, Env env) throws FisherException {
		Seal labelSeal = syn.label == null ? SealMaker.ofUnlabelledLoop(syn, env.container) : SealMaker.ofLoop(
				syn.label, syn, env.container);
		syn.loopControlSeal = labelSeal;
		assert (syn.label == null || syn.label.seal() == labelSeal);
		assert (syn.loopControlSeal != null);

		// We need an inner env for the label of the loop.
		Env loopEnv = env.innerWithNewContainer(null);
		if (syn.label != null)
			loopEnv.define(syn.label.str(), labelSeal);
		return loopEnv;
	}

	private Env sealUntil(While syn, Env outerEnv, Env loopEnv) throws FisherException {
		// syn = until(B) C -- or maybe do C until(B).

		// Bindings in C stay there.
		Env envC = loopEnv.inner();
		sealThus(envC, syn.body);

		// B guards the consequent, so bindings in B go to the consequent.
		sealThus(outerEnv, syn.test);
		return outerEnv;
	}

	private Env sealDoWhile(While syn, Env outerEnv, Env loopEnv) throws FisherException {
		// syn = do C while B
		// C and B are in unrelated scopes.
		Env envB = loopEnv.inner();
		sealThus(envB, syn.test);
		Env envC = loopEnv.inner();
		sealThus(envC, syn.body);
		return outerEnv;
	}

	private Env sealWhile(While syn, Env outerEnv, Env loopEnv) throws FisherException {
		// syn = while(C)B
		// Bindings in C are available in B
		Env inner = loopEnv.inner();
		sealThus(inner, syn.test, syn.body);
		return outerEnv;
	}

	@Override
	public Env visit(Break syn, Env env) throws FisherException {
		if (syn.id != null) {
			Seal seal = env.seal(syn.id, syn);
			syn.id.setSeal(seal, syn);
			syn.loopSeal = seal;
		} else {
			Seal encSeal = sealOfEnclosingLoop(syn, env, true);
			syn.loopSeal = encSeal;
		}
		assert (syn.loopSeal != null || Compilation.isFlagged(syn));
		return env;
	}

	@Override
	public Env visit(Continue syn, Env env) throws FisherException {
		if (syn.id != null) {
			Seal seal = env.seal(syn.id, syn);
			syn.id.setSeal(seal, syn);
			syn.loopSeal = seal;
		} else {
			Seal encSeal = sealOfEnclosingLoop(syn, env, true);
			syn.loopSeal = encSeal;
		}
		assert (syn.loopSeal != null || Compilation.isFlagged(syn));
		return env;
	}

	private Seal sealOfEnclosingLoop(Syntax stmt, Env env, boolean first) throws FisherException {
		if (stmt instanceof While) {
			While wh = (While) stmt;
			return wh.loopControlSeal;
		} else if (stmt instanceof For) {
			For fo = (For) stmt;
			return fo.loopControlSeal;
		} else {
			Syntax parent = stmt.parent();
			if (parent != null) {
				Seal s = sealOfEnclosingLoop(parent, env, false);
				if (s != null)
					return s;
				else if (s == null && !first) {
					return null;
				} else /* s == null and first -- this is the statement to flag. */{
					stmt.flag(DangerLevel.ERROR, "There is no loop here to break or continue.", "");
					return null;
				}
			} else {
				return null;
			}
		}
	}// sealOfEnclosingLoop

	@Override
	public Env visit(If syn, Env env) throws FisherException {
		if (BindingProducingTester.producesBindings(syn.test)) {
			return doIfProducingBindings(syn, env);
		} else {
			return doIfWithoutProducingBindings(syn, env);
		}
	}

	private Env doIfProducingBindings(If syn, Env env) throws FisherException {
		if (syn.reallyUnless) {
			Env innerThen = env.inner();
			sealThus(innerThen, syn.Then);
			Env innerElse = env.inner();
			sealThus(innerElse, syn.test, syn.Else);
			return env;

		} else {
			// really if
			Env innerThen = env.inner();
			sealThus(innerThen, syn.test, syn.Then);
			Env innerElse = env.inner();
			sealThus(innerElse, syn.Else);
			return env;
		}
	}

	private Env doIfWithoutProducingBindings(If syn, Env env) throws FisherException {
		sealThus(env.inner(), syn.test);
		sealThus(env.inner(), syn.Then);
		sealThus(env.inner(), syn.Else);
		return env;
	}

	@Override
	public Env visit(MatchExp syn, Env env) throws FisherException {
		sealThus(env, syn.subject);
		sealThus(env, syn.pat);
		return env;
	}

	@Override
	public Env visit(PatAnd syn, Env arg) throws FisherException {
		return sealThus(arg, syn.subpats);
	}

	@Override
	public Env visit(PatEvalTestExp syn, Env env) throws FisherException {
		Env inner = env.inner();
		sealThus(inner, syn.exp);
		return env;
	}

	@Override
	public Env visit(ItExp syn, Env env) throws FisherException {
		return env;
	}

	@Override
	public Env visit(PatInterpolation syn, Env arg) throws FisherException {
		Env inner = arg.inner();
		sealThus(inner, syn.exp);
		return arg;
	}

	@Override
	public Env visit(PatMatchSomethingElse syn, Env env) throws FisherException {
		return sealThus(env, syn.exp, syn.pat);
	}

	@Override
	public Env visit(PatNot syn, Env arg) throws FisherException {
		Env inner = arg.inner();
		sealThus(inner, syn.subpat);
		return arg;
	}

	@Override
	public Env visit(PatNotNull syn, Env arg) throws FisherException {
		return sealThus(arg, syn.subpat);
	}

	@Override
	public Env visit(PatOr syn, Env env) throws FisherException {
		sealIndependently(syn.subpats, env);
		return env;
	}

	@Override
	public Env visit(PatWildcard syn, Env arg) throws FisherException {
		return arg;
	}

	@Override
	public Env visit(PatRecordCtor syn, Env env) throws FisherException {
		return sealThus(env, syn.fields);
	}

	@Override
	public Env visit(PatMethodCall syn, Env env) throws FisherException {
		Seal methSeal = SealMaker.of(syn, env.container);
		syn.methodName.setSeal(methSeal, syn);
		return sealThus(env, syn.subpat);
	}

	@Override
	public Env visit(PatExtract syn, Env env) throws FisherException {
		Seal typeSeal = sealQualName(syn.patname, env); // WAS: env.seal(syn.patname, syn);
		if (typeSeal == null) {
			syn.flag(DangerLevel.ERROR, "Undefined extractor: " + syn.patname, "");
			return env;
		}
		if (typeSeal.kind != SealKind.CLASS && typeSeal.kind != SealKind.JAVALY_CLS) {
			syn.flag(DangerLevel.ERROR, "Extraction patterns only work for classes", "and " + syn.patname + " is a "
					+ typeSeal.kind);
		}
		//sealQualName does this (I think)	syn.patname.setSeal(typeSeal, syn);
		return sealThus(env, syn.subpats);
	}

	@Override
	public Env visit(PatRecordField syn, Env arg) throws FisherException {
		Seal lblSeal = SealMaker.of(syn, null);
		// But that label isn't bound to anything really.
		return sealThus(arg, syn.pat);
	}

	@Override
	public Env visit(Match syn, Env env) throws FisherException {
		sealIndependently(syn.subject, env);
		sealIndependently(syn.cases, env);
		return env;
	}

	@Override
	public Env visit(Case syn, Env env) throws FisherException {
		sealThus(env, syn.pat, syn.from, syn.envelope, syn.body);
		return env;
	}

	@Override
	public Env visit(Return syn, Env arg) throws FisherException {
		sealIndependently(syn.exp, arg);
		return arg;
	}

	@Override
	public Env visit(Throw syn, Env env) throws FisherException {
		sealIndependently(syn.exn, env);
		return env;
	}

	@Override
	public Env visit(Try syn, Env env) throws FisherException {
		sealIndependently(syn.body, env);
		sealIndependently(syn.cases, env);
		sealIndependently(syn.fin, env);
		return env;
	}

	@Override
	public Env visit(RecordCtor syn, Env env) throws FisherException {
		sealIndependently(syn.fields, env);
		return env;
	}

	@Override
	public Env visit(RecordField syn, Env env) throws FisherException {
		Seal fieldNameSeal = SealMaker.ofFieldName(syn.id, syn, null);
		syn.id.setSeal(fieldNameSeal, syn);
		sealIndependently(syn.exp, env);
		return env;
	}

	@Override
	public Env visit(ModuleFileImport syn, Env arg) throws FisherException {
		return sealThus(arg, syn.imp);
	}

	@Override
	public Env visit(StringWithInterpolations syn, Env arg) throws FisherException {
		return sealThus(arg, syn.bits);
	}

	@Override
	public Env visit(StringBitText syn, Env arg) throws FisherException {
		return arg;
	}

	@Override
	public Env visit(StringBitVar syn, Env arg) throws FisherException {
		return sealThus(arg, syn.exp);
	}

	@Override
	public Env visit(SuperThingie syn, Env env) throws FisherException {
		// super@A.f() and new@A.f() can only be done inside a class.
		ClassStatic currCS = env.classStatic();
		if (currCS == null) {// grack, I wanna have a breakpoint here.			
			assert (currCS != null);
		}

		ClassStatic superCS = null;
		QualName cls = syn.cls;
		Seal superseal;
		if (cls == null) {
			int nSupers = currCS.superclasses.size();
			if (nSupers != 1) {
				syn.flag(DangerLevel.ERROR,
						"super.f() and new() are only allowed when there is exactly one superclass", "And there are "
								+ nSupers);
				return env;
			}
			ClassStatic superclassCS = currCS.superclasses.get(0);
			superseal = superclassCS.classSeal;
		} else {
			superseal = env.seal(syn.cls, syn);

		}

		if (superseal == null) {
			syn.flag(DangerLevel.ERROR, "Not defined: " + syn.cls, "");
			return env;
		}
		// superseal ought to be a seal for a class.
		// but it might be an alias, so check its kind:
		if (superseal.kind == SealKind.CLASS) {
			SealForClass superclassSeal = (SealForClass) superseal.dealias();
			superCS = superclassSeal.classStatic();
			if (syn.cls != null)
				sealQualName(syn.cls, env);
			syn.sealForSuperclass = superseal;
		} else {
			syn.flag(DangerLevel.ERROR, "Not a class:" + cls, "(but it is a " + superseal.kind + ")", superseal);
			return env;
		}

		assert (superCS != null);

		// current class != null

		if (!env.anywhereInsideAClass()) {
			syn.flag(DangerLevel.ERROR, "supercall outside of class", "");
			return env;
		}

		// superclass is a parent of current class.

		if (!currCS.superclasses.contains(superCS)) {
			syn.flag(DangerLevel.ERROR, "Not a superclass.", superCS + " is not a superclass of " + currCS, currCS,
					superCS);
			return env;
		}

		// stuff info somewhere

		syn.superClassStatic = superCS;

		sealThus(env, syn.args);

		return env;
	}

	@Override
	public Env visit(SuperCall syn, Env env) throws FisherException {
		this.visit((SuperThingie) syn, env);
		Seal methSeal = SealMaker.of(syn, env.container);
		syn.method.setSeal(methSeal, syn);

		return env;
	}

	@Override
	public Env visit(SuperCtorCall syn, Env arg) throws FisherException {
		if (syn.isCallToADifferentCtorForThis()) {
			// new(x,y,z) -- basically a function call, only without needing to name the function.
			// Oh, and we're going to need the classStatic too: 
			syn.classStaticForCurrentClass = arg.classStatic();
			return sealThus(arg, syn.args);
		} else {
			// new@A(x,y,z) -- a tricky thing, generally like super@A.f(x,y,z)
			return this.visit((SuperThingie) syn, arg);
		}
	}

	@Override
	public Env visit(QueryAbstract syn, Env arg) throws FisherException {
		sealIndependently(syn.actualCode, arg);
		return arg;
	}

	@Override
	public Env visit(OpABExp syn, Env env) throws FisherException {
		sealIndependently(syn.target, env);
		sealIndependently(syn.amount, env);
		return env;
	}

	@Override
	public Env visit(Table syn, Env env) throws FisherException {

		List<TableFields> vals = syn.vals;
		List<TableKey> keys = syn.keys;

		dealWithAbstractTable(syn, env, vals, keys);

		return env;
	}// visit Table

	@Override
	public Env visit(Ord syn, Env arg) throws FisherException {

		dealWithAbstractTable(syn, arg, syn.fields, null);

		return arg;
	}

	private void dealWithAbstractTable(AbstractTable syn, Env env, List<TableFields> vals, List<TableKey> keys)
			throws FisherException, FisherInternalCompilerDoom {
		List<ColInfo> infos = new ArrayList<ColInfo>();
		List<ColInfo> keyInfos = new ArrayList<ColInfo>();
		ColInfo mapColInfo = null;
		int i = 0;

		if (keys != null) {
			for (TableKey tkey : keys) {
				Id key = tkey.name;
				ColInfo ki = new ColInfo(key.str(), ColAccess.KEY, ColSpecial.NORMAL, i++, null, tkey.typeConstraints);
				infos.add(ki);
				keyInfos.add(ki);
				Seal keySeal = SealMaker.ofColumn(key, key);
				sealThus(env, tkey.typeConstraints);
				key.setSeal(keySeal, key);
			}
		} else /* special case for ord */{
			// It's an ord, I hope.
		}

		for (TableMember member : vals) {
			if (member instanceof TableFields) {
				TableFields tfs = (TableFields) member;
				for (IdWithOptInit iwoi : tfs.idInits) {
					ColInfo ci = new ColInfo(iwoi.id.str(), tfs.colAccess, tfs.colSpecial, i++, iwoi.init,
							iwoi.typeConstraints);
					sealThus(env, iwoi.typeConstraints);
					infos.add(ci);
					sealThus(env, iwoi.init);
					Seal colSeal = SealMaker.ofColumn(iwoi.id, iwoi);
					// sealMaker saves it
					// And it doesn't go into the env.
					if (tfs.colAccess == ColAccess.KEY) {
						Doom.internal("Obsolete syntactic concept has snuck into present day!", syn);
					}
					if (tfs.colSpecial == ColSpecial.MAP) {
						if (mapColInfo == null) {
							mapColInfo = ci;
						} else {
							iwoi.flag(DangerLevel.ERROR, "A table can have only one 'map' field?", "");
						}
					}
				}
			} else {
				Doom.internal("Unknown kind of table member: " + member, syn, member);
			}
		}// for

		ColInfo[] colInfos = new ColInfo[infos.size()];
		infos.toArray(colInfos);
		syn.colInfos = colInfos;
		ColInfo[] kis = new ColInfo[keyInfos.size()];
		keyInfos.toArray(kis);
		syn.keyInfos = kis;

		List<ColInfo> nonkeys = new ArrayList<ColInfo>(infos);
		nonkeys.removeAll(keyInfos);
		ColInfo[] nonkeysa = new ColInfo[nonkeys.size()];
		nonkeys.toArray(nonkeysa);
		syn.nonkeyInfos = nonkeysa;

		syn.mapColInfo = mapColInfo;
	}

	@Override
	public Env visit(BracketCall syn, Env arg) throws FisherException {
		return sealThus(arg, syn.function, syn.args);
	}

	@Override
	public Env visit(MapCtor syn, Env arg) throws FisherException {
		return sealThus(arg, syn.actualCode);
	}

	@Override
	public Env visit(Spawn syn, Env env) throws FisherException {
		Env inner = env.inner();
		if (syn.name != null) {
			syn.name.setSeal(SealMaker.of(syn, null), syn);
		}
		List<Cmd> topCrap = Bard.list();
		topCrap.addAll((List<Cmd>) (List) syn.localMembers);
		topCrap.addAll(syn.funs());
		topCrap.addAll((List<Cmd>) (List) syn.highLevelCommunications);
		//		sealThus(inner, syn.localMembers);
		//		inner = sealSeq(inner, syn.funs, true);
		//		sealThus(inner,  syn.highLevelCommunications, syn.init, syn.body);

		inner = sealSeq(inner, topCrap, true);
		sealThus(inner, syn.init, syn.body);

		// Now, check that the variables used here are proper...
		Set<Seal> fv = FreeVariableSeals.of(syn);
		// Record them -- we'll use them when spawning.
		// (Note that syn.freeVarSeals() is initialized to an empty set, not null.) 
		for (Seal seal : fv) {
			if (!suitableForSpawnedComponent(seal)) {
				syn.flag(DangerLevel.ERROR, seal.str + " is a " + seal.kind
						+ " and thus can't be given to a spawned component.", "");
			} else if (shouldBeSnaggedOnSpawning(seal, syn)) {
				syn.freeVarSeals().add(seal);
			}
		}
		return env;
	}

	private boolean suitableForSpawnedComponent(Seal seal) {
		// Thing from modules *are* suitable -- the module will get instantiated per component.
		if (seal.container != null && seal.container.kind == SealKind.MODULE)
			return true;
		// DEAD is OK
		if (seal.kind == SealKind.DEAD)
			return true;
		// Otherwise, it goes by the kind.
		return SealKind.suitableForSpawnedComponent.contains(seal.kind);
	}

	//	int temphack = 0;

	private boolean shouldBeSnaggedOnSpawning(Seal seal, Spawn spawn) {
		// Only certain kinds of seals get imported on spawning.
		if (!SealKind.shouldBeImportedOnSpawning.contains(seal.kind))
			return false;
		// And, stuff that comes from modules doesn't.
		//		if (seal.str().startsWith("refe")) {
		//			System.err.println("Kludge to check stuff");
		//			return true;
		//		}
		if (seal.container != null && seal.container.kind == SealKind.MODULE) {
			//			System.err.println("Wrongly? importing stuff on spawning");
			// I believe that stuff should be importedOnSpawning if 
			// the module definition was outside of the spawn that encloses the reference.
			//			if (seal.str().startsWith("refe")) {
			//				temphack += 1;
			//				System.err.println("Temp refe hack happening - " + temphack);
			//				return (temphack == 1);
			//			}
			if (importStatementGettingSealIsOutsideOfCurrentSpawn(seal, spawn))
				return true;
			return false;
		}
		if (seal.kind == SealKind.DEAD)
			return false;
		return true;
	}

	private static boolean importStatementGettingSealIsOutsideOfCurrentSpawn(Seal seal, Spawn spawn) {
		assert (seal.container != null && seal.container.kind == SealKind.MODULE);
		ImportStmt importstmt = seal.getImportStmt();
		if (importstmt == null) {
			return false;
			//			spawn.flag(DangerLevel.INTERNAL_ERROR, "A field of a module doesn't have its import statement listed", "", "seal=" + seal, "spawn=" + spawn);
			//			throw new RuntimeException("Speciel [sic] doom of the day! - "+ seal + " --- "+ spawn);
		}
		if (spawn.computeDescendants().contains(importstmt))
			return false;
		return true;
	}

	@Override
	public Env visit(ProcBody syn, Env arg) throws FisherException {
		sealIndependently(syn.bodycode, arg);
		return arg;
	}

	@Override
	public Env visit(ProcInit syn, Env arg) throws FisherException {
		sealIndependently(syn.initcode, arg);
		return arg;
	}

	@Override
	public Env visit(SyncDecl syn, Env env) throws FisherException {
		Seal commSeal = SealMaker.of(syn, null);
		syn.name.setSeal(commSeal, syn);
		Env inner = env.inner();
		return sealMethodlyThing(inner, syn.name, syn.funbody);
		//		 produceTrashSealsForDeadCode(syn);
		//		 return env;
	}

	@Override
	public Env visit(AsyncDecl syn, Env env) throws FisherException {
		Seal commSeal = SealMaker.of(syn, null);
		syn.name.setSeal(commSeal, syn);
		return sealMethodlyThing(env, syn.name, syn.funbody);
		//		 produceTrashSealsForDeadCode(syn);
		//		 return env;
	}

	@Override
	public Env visit(Send syn, Env arg) throws FisherException {
		sealThus(arg, syn.receiver, syn.exp, syn.security);
		return arg;
	}

	@Override
	public Env visit(Recv syn, Env env) throws FisherException {
		sealIndependently(syn.cases, env);
		sealIndependently(syn.timeoutLen, env);
		sealIndependently(syn.timeoutCmd, env);
		return env;
	}

	@Override
	public Env visit(AsyncStmt syn, Env env) throws FisherException {
		return sealThus(env, syn.actualCode);
	}

	@Override
	public Env visit(SyncStmt syn, Env env) throws FisherException {
		return sealThus(env, syn.actualCode);
	}

	@Override
	public Env visit(Serve syn, Env arg) throws FisherException {
		return sealThus(arg, syn.actualCode);
	}

	@Override
	public Env visit(JavalyFun syn, Env arg) throws FisherException {
		assert (syn.name.seal() != null);
		return arg;
	}

	private void trash(List<Id> ids, Syntax syn) throws FisherException {
		for (Id id : ids) {
			id.setSeal(SealMaker.ofIdInDeadCode(id.str()), syn);
		}
	}

	public Env visit(JavalyClassDecl syn, Env env) throws FisherException {
		//		// The class name is a CLS -- this should be done by ExtractSealsFromModule
		//		Seal clsSeal = SealMaker.of(syn, env.container);
		//		syn.name.setSeal(clsSeal, syn);
		String clsname = syn.name.str();
		checkForShadowing(syn, env, clsname);
		env.define(clsname, syn.name.seal());
		//		syn.name.setSeal(clsSeal, syn);

		// I am lazy!  Trash all the other seals.

		trash(syn.impl.ids, syn);

		for (JavalyMethodDecl jam : syn.methods) {
			trash(Bard.list(jam.name, jam.impl), jam);
			trash(jam.formals, jam);
		}
		for (JavalyNewDecl jnu : syn.ctors) {
			trash(Bard.list(jnu.name), jnu);
			trash(jnu.formals, jnu);
		}
		trash(syn.fields, syn);
		return env;
	}

	@Override
	public Env visit(CmdsInAList syn, Env env) throws FisherException {
		// Sort of like sealing a seq, only 
		// Carefully propagate 'new'-ness, from the monobody of 'new C(x){v=x;}'
		// to the 'v=x;' part.
		SealCircularDefs.seal(syn.cmds, env);
		// Second pass, to get all the details
		sealInOrder(syn.cmds, env);
		return env;
	}

	@Override
	public Env visit(ClassFormal syn, Env env) throws FisherException {
		sealThus(env, syn.typeConstraints);
		return sealSimple(syn, env, syn.name, syn.name.str());

	}

	@Override
	public Env visit(ComponentDecl syn, Env arg) throws FisherException {
		Seal nameSeal = SealMaker.ofIdInDeadCode(syn.name.str());
		syn.name.setSeal(nameSeal, syn);
		return sealThus(arg, syn.actualCode);
	}

	@Override
	public Env visit(SpawnByComponentName syn, Env arg) throws FisherException {
		sealThus(arg, syn.exp, syn.args);
		return arg;
	}

}
