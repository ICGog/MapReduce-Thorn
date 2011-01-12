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
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import sun.reflect.MethodAccessor;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.Frame;
import fisher.runtime.ObjectTh;
import fisher.runtime.Thing;
import fisher.statics.purity.PurityStatus;
import fisher.statics.purity.StaticPurityChecker;
import fisher.syn.Bind;
import fisher.syn.ClassFormal;
import fisher.syn.ClsCtorDef;
import fisher.syn.ClsDecl;
import fisher.syn.ClsExtends;
import fisher.syn.ClsPatDef;
import fisher.syn.ImportStmt;
import fisher.syn.MethDecl;
import fisher.syn.MonoBody;
import fisher.syn.Pat;
import fisher.syn.PatVar;
import fisher.syn.QualName;
import fisher.syn.VarDecl;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.syn.interfaces.ClassMember;
import fisher.syn.interfaces.Classlike;
import fisher.syn.queries.PatVars_Defined_By_Pattern;
import fisher.syn.queries.StringsDefinedByFormals;
import fisher.syn.queries.SynUtil;
import fisher.util.Bard;
import fisher.util.DangerLevel;
import fisher.util.Doom;
import fisher.util.Fisher;
import fisher.util.FisherException;
import fisher.util.SpecialCharacters;

public class ClassStatic {
	static String copyright() {
		return fisher.util.Copyright.IBM_COPYRIGHT;
	}

	public final Map<MethodSig, MethDecl> methods = new HashMap<MethodSig, MethDecl>();
	public final Map<String, ClsPatDef> pats = new HashMap<String, ClsPatDef>(0);
	public final List<ClassMember> instanceInitCode = new ArrayList<ClassMember>(0);
	public final Set<String> fieldNames;
	public final Set<Seal> fieldSeals = new HashSet<Seal>();
	//	public final Map<String, Seal> fieldName2Seal = new HashMap<String, Seal>();
	public final List<String> definedNames = new ArrayList<String>();
	public final List<ClsCtorDef> constructors;
	public final Classlike clsDecl;
	public final boolean hasDefaultConstructor;
	public final SealForClass classSeal;
	public final List<ClassStatic> superclasses;
	public final List<String> fieldNamesOfDefaultConstructor;

	/*
	 * PurityStatus needs to be computed lazily.  ClassStatics are constructed during sealing,
	 * and purity status requires a completely-sealed class, so that can't happen 'til later.
	 * So: purityStatus == null means "not computed yet."
	 */
	private PurityStatus purityStatus = null;

	public ClassStatic(Classlike clsDecl, Env env) throws FisherException {
		this.clsDecl = clsDecl;
		this.constructors = snagConstructors(clsDecl);
		boolean hasFormals = clsDecl.params() != null;

		this.hasDefaultConstructor = (constructors.size() == 0) || !(hasFormals);
		this.classSeal = clsDecl.classSeal();
		this.fieldNames = new HashSet<String>();
		this.superclasses = snagSuperclasses(clsDecl, env);
		this.snagMethodsPatsVarsVals();
		this.fieldNamesOfDefaultConstructor = getFieldNamesOfDefaultConstructor(clsDecl);
		//		System.out.println("ClassStatic.ctor: " + clsDecl);
	}

	//	public Seal getSealForField(String fieldName) {
	//		return fieldName2Seal.get(fieldName);
	//	}

	public Set<String> namesOfFieldsDefinedByThisClassButNotParents() {
		Set<String> S = new HashSet<String>(fieldNames);
		for (ClassStatic superclass : this.superclasses) {
			S.removeAll(superclass.fieldNames);
		}
		return S;
	}

	private static List<String> getFieldNamesOfDefaultConstructor(Classlike clsDecl) {
		if (clsDecl.params() == null)
			return null;
		List<String> L = new ArrayList<String>();
		for (ClassFormal cf : clsDecl.params()) {
			L.add(cf.name.str());
			//			if (p instanceof PatVar) {
			//				PatVar pv = (PatVar) p;
			//				L.add(pv.id.str());
			//			}
			//			else {
			//				p.flag(DangerLevel.ERROR, "Class formals must be single identifiers", "(Just a variable, human)");
			//			}
		}
		return L;
	}

	// This method takes care of the formals and superclasses 
	// (1) Return the list of superclass ClassStatics
	// (2) add fieldNames for the fields that come from the formals
	//     E.g., class C(x,y,z) extends D(x) ----> needs y and z, not x, as new val fields.
	// (3) updates DefinedNames ditto.
	private List<ClassStatic> snagSuperclasses(Classlike clsDecl, Env env) throws FisherException {

		//Set<String> namesInFormals = StringsDefinedByFormals.of(clsDecl.formals());

		List<ClsExtends> extendses = clsDecl.extendses();
		List<ClassStatic> supers = new ArrayList<ClassStatic>(extendses.size());
		for (ClsExtends ext : extendses) {
			QualName classNameBeingExtended = ext.superName;
			// (1) Get the seal
			Seal seal1 = env.seal(classNameBeingExtended, (Syntax) clsDecl);
			if (seal1 == null) {
				ext.flag(DangerLevel.ERROR, "No definition for superclass", classNameBeingExtended
						+ " is not defined here.");
				continue;
			}
			Seal superSeal = seal1.dealias();
			if (superSeal instanceof SealForClass) {
				SealForClass superclassSeal = (SealForClass) superSeal;
				ClassStatic cs = superclassSeal.classStatic();
				if (cs == null) {
					ext.flag(DangerLevel.ERROR, "Static definitions for " + seal1 + " are not visible here.  Perhaps its class declaration is done after this one? ", "");
				} else {
					supers.add(cs);

					// Import all the field seals and names from the superclass,
					// because these are keys to the instance variable table.
					this.fieldSeals.addAll(cs.fieldSeals);
					// Import all the names from the superclass, 
					// if we're in a language variant in which fields are protected or public: 
					this.fieldNames.addAll(cs.fieldNames);
				}
			} else {
				((Syntax) clsDecl).flag(DangerLevel.ERROR, "Not a class, so it can't be extended: "
						+ classNameBeingExtended, "");
			}

			// (2) Remove names from D(x) 
			// TODO -- as a temporary measure, we're assuming no arguments to D.
			Set<String> namesInExt = Collections.EMPTY_SET;

			// namesInExt will include D too, but that's OK.
			// namesInFormals.removeAll(namesInExt);

		}

		// OK, namesInFormals now is the names in the formals not used by a supercall.

		//this.fieldNames.addAll(namesInFormals);
		//this.definedNames.addAll(namesInFormals);

		return supers;
	}

	private void snagMethodsPatsVarsVals() throws FisherException {
		String className = classSeal.str;
		for (ClassMember cm : clsDecl.members()) {
			if (cm instanceof MethDecl) {
				MethDecl method = (MethDecl) cm;
				String name = method.name.str();
				// Don't check for collision -- two methods *can* have the same name.
				// checkCollision(name, cm, className);
				MethodSig sig = new MethodSig(name, SynUtil.arity(method));
				if (this.methods.containsKey(sig)) {
					cm.flag(DangerLevel.ERROR, "Duplicate definition for " + sig + " as a member of " + className,
							"There are two or more definitions of " + sig + ", one of which is " + method);
				}
				this.methods.put(sig, method);
			} else if (cm instanceof ClsPatDef) {
				ClsPatDef pat = (ClsPatDef) cm;
				String name = pat.id.str();
				checkCollision(name, cm, className);
				this.pats.put(name, pat);
			} else if (cm instanceof VarDecl) {
				VarDecl varf = (VarDecl) cm;
				String name = varf.var.str();
				this.fieldNames.add(name);
				checkCollision(name, cm, className);
				instanceInitCode.add(varf);
			} else if (cm instanceof Bind) {
				Bind valf = (Bind) cm;
				List<PatVar> patVars = PatVars_Defined_By_Pattern.of(valf.pat);
				for (PatVar patVar : patVars) {
					String name = patVar.id.str();
					this.fieldNames.add(name);
					checkCollision(name, cm, className);
				}
				instanceInitCode.add(valf);
			}

			else if (cm instanceof ClsCtorDef) {
				ClsCtorDef cd = (ClsCtorDef) cm;
				// ignore it.
			} else if (cm instanceof ImportStmt) {
				ImportStmt impStmt = (ImportStmt) cm;
				// ignore it.
			} else {
				Doom.internal("Confusing kind of Classember", (Syntax) cm);
			}

		}
	}

	public void runConstructorCode(ObjectTh oth, Thing[] args, Evaller evaller, Syntax src) throws FisherException {
		if (this.constructors.isEmpty()) {
			doNullaryCtorsOfAllSuperclasses(oth, evaller, src);
			return;
		}
		constructorCall: for (ClsCtorDef ctorDef : this.constructors) {
			MonoBody monobody = ctorDef.monobody;
			Frame ctorFrame = Frame.inner(oth);
			if (evaller.evalFormals(monobody.formals, args, ctorFrame)) {
				Thing res = evaller.eval(monobody.body, ctorFrame);
				return;
			}
		}

		Doom.runtime("No constructor matches.", src, "args = " + Bard.sep(args, ", "));
	}

	private void doNullaryCtorsOfAllSuperclasses(ObjectTh oth, Evaller evaller, Syntax src) throws FisherException {
		for (ClassStatic superclass : this.superclasses) {
			superclass.runConstructorCode(oth, EvalUtil.NO_ARGS, evaller, src);
		}
	}

	private void checkCollision(String name, ClassMember classMember, String className) throws FisherException {
		if (definedNames.contains(name)) {
			classMember.flag(DangerLevel.ERROR, "Duplicate definition for " + name + " as a member of " + className,
					"There are two or more definitions of " + name + ", one of which is " + classMember, classMember);
		}
		definedNames.add(name);
	}

	private static List<ClsCtorDef> snagConstructors(Classlike clsDecl) {
		List<ClsCtorDef> ctors = new ArrayList<ClsCtorDef>();
		for (ClassMember cm : clsDecl.members()) {
			if (cm instanceof ClsCtorDef) {
				ClsCtorDef ctor = (ClsCtorDef) cm;
				ctors.add(ctor);
			}
		}

		return ctors;
	}// snagConstructors

	/**
	 * @param id
	 * @return the MethDecl whose name is id, or null if there isnt' one.
	 */
	public MethDecl findMethodOrNull(MethodSig sig) {
		return internalFindMethodOrNull(sig);
	}

	public ClsCtorDef findCtorOrNull(int arity) {
		// Unlike finding methods, this does *not* chain up to superclasses.
		for (ClsCtorDef ccd : this.constructors) {
			if (ccd.monobody.formals.formals.size() == arity)
				return ccd;
		}
		return null;
	}

	private MethDecl internalFindMethodOrNull(MethodSig sig) {
		if (methods.containsKey(sig))
			return methods.get(sig);
		for (ClassStatic superclass : this.superclasses) {
			MethDecl sm = superclass.internalFindMethodOrNull(sig);
			if (sm != null)
				return sm;
		}
		return null;
	}

	public String toString() {
		return SpecialCharacters.CLASS_STATIC + classSeal.str;
	}

	public Set<MethodSig> sigsOfMethodsDefinedLocally() {
		Set<MethodSig> S = new HashSet<MethodSig>();
		S.addAll(methods.keySet());
		return S;
	}

	public Set<MethodSig> sigsOfApplicableMethods() {
		Set<MethodSig> S = sigsOfMethodsDefinedLocally();
		for (ClassStatic csuper : superclasses) {
			S.addAll(csuper.sigsOfApplicableMethods());
		}
		return S;
	}

	public PurityStatus purityStatus() throws FisherException {
		if (purityStatus == null) {
			StaticPurityChecker.check(clsDecl); // 
			this.purityStatus = clsDecl.purityStatus();
		}
		return this.purityStatus;
	}

	private String cachedSerializationKey = null;

	public static final String header = "ClassStatic{";
	public static final String tailer = "}";

	public String serializationKey() {
		if (cachedSerializationKey != null)
			return cachedSerializationKey;
		String s1 = "ClassStatic{";
		String s2 = this.toString().hashCode() + ""; // Yay, hashcode is precisely specified.
		String s3 = "/";
		String s4 = this.classSeal.toString();
		String s5 = "}";
		cachedSerializationKey = s1 + Bard.serStr(s2) + s3 + Bard.serStr(s4) + s5;
		//System.err.println("Ser key is " + cachedSerializationKey + " for " + this);
		return cachedSerializationKey;
	}

}
