
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.statics.purity;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import fisher.desugar.AnonObjDesugarer;
import fisher.statics.ClassStatic;
import fisher.statics.PredefinedIdentifiers;
import fisher.statics.Seal;
import fisher.statics.SealKind;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.syn.interfaces.ClassMember;
import fisher.syn.interfaces.Classlike;
import fisher.syn.interfaces.Puretic;
import fisher.syn.queries.FreeIds;
import fisher.syn.queries.IdsAppearingIn;
import fisher.syn.visitor.PureticWalker;
import fisher.util.Bard;
import fisher.util.DangerLevel;
import fisher.util.Doom;
import fisher.util.FisherException;
import fisher.syn.*;
import static fisher.util.Bard.*;
import static fisher.statics.SealKind.*;
import static fisher.statics.purity.PurityStatus.*;
import static fisher.syn.interfaces.Puretic.*;

public  class  StaticPurityChecker implements PureticWalker<StaticPurityChecker.PurityContext, FisherException>  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	// I don't currently have a use for the walker's second argument, 
	// so I'm making it a private static class with no contents.
	// It can be replaced more easily than just Object, as I learned to my sorrow
	// elsewhere.
	protected static class PurityContext{
		public ClsDecl enclosingClsDecl;
		public Set<Seal> valSealsFromSurroundingClsDecl;
		public PurityContext(ClsDecl enclosingClsDecl, Set<Seal> valSealsFromSurroundingClsDecl) {
			super();
			this.enclosingClsDecl = enclosingClsDecl;
			this.valSealsFromSurroundingClsDecl = valSealsFromSurroundingClsDecl;
		}
		public String toString() {
			return "PC{" + Bard.sep(valSealsFromSurroundingClsDecl, ",") + "}";
		}

		
		
	}
	
	private static PurityContext outsideOfClassContext = new PurityContext(null, null);
	
	private static StaticPurityChecker IT = new StaticPurityChecker();
	
	private static Set<Puretic> beingChecked = new HashSet<Puretic>();
	
	
	public static void check(Puretic pur) throws FisherException {
		beingChecked.clear();
		checkInternal(pur);
	}
	
	public static void checkInternal(Puretic pur) throws FisherException {
		// Don't check repeatedly
//		if (beingChecked.contains(pur)) {
//			Doom.internal("Recursive purity check of " + pur, (Syntax)pur);
//		}
		
		if (pur.purityStatus() != UNCHECKED) return;
		pur.setPurityStatus(INNOCENT_TIL_PROVEN); // This is how it starts.
//		beingChecked.add(pur);
		pur.accept(IT, outsideOfClassContext);
		
		if (pur.purityStatus() == INNOCENT_TIL_PROVEN) {
			Doom.internal("Purity status not resolved: ", pur);
		}
		
//		System.err.println("StaticPurityChecker.checkInternal(" + pur + ":" + Bard.shortClassName(pur) + ") := " + pur.purityStatus() );
//		beingChecked.remove(pur);
	}
	
	public static boolean referenceIsAllowedFromPureTo(Puretic pur) throws FisherException{
		switch (pur.purityStatus()) {
		case IMPURE:
		case ERROR:
			return false;
		case PURE:
		case INNOCENT:
		case INNOCENT_TIL_PROVEN: // I am not 100% sure about this -- it might become wrong later.
			// But this is only used to purity check a method inside a class where the method
			// refers to the class -- if the class is judged impure, the purity of the method
			// isn't relevant.  I hope.
			return true;
		case UNCHECKED:
			Doom.internal("Hey, you should have checked this for purity already!", (Syntax)pur);
			return false;
		default:
			Doom.internal("Someone didn't update StaticPurityChecker.referenceIsAllowedFromPureTo", (Syntax)pur);
			return false;
		}
	}
	
	
	private static List<SealKind> inherentlyPureSeals = list(MODULE, PROBE, PAT_SEAL, PAT_FORMAL, FIELD_REF, METHOD_REF, FIELD_NAME, SealKind.IT, BUILT_IN_TYPE, COLUMN, DEAD);
	private static List<SealKind> conditionallyPureSeals = list(CLASS, FUN, JAVALY_FUN, JAVALY_CLS);
	
	private static boolean sealedWithPurity(Seal seal, Puretic pur) throws FisherException {
		if (seal == null) {
			Doom.internal("Seal is null in sealedWithPurity", (Syntax) pur);
		}
		if (inherentlyPureSeals.contains(seal.kind)) {
			return true;
		}
		else if( conditionallyPureSeals.contains(seal.kind)){
			Syntax def = seal.def;
			if (def == null) {
				Doom.internal("Seal has null definition " + seal, (Syntax)pur, seal);
				return false; // never reached
			}
			else if (def instanceof Puretic) {
				Puretic defpur = (Puretic) def;
				checkInternal(defpur);
				return referenceIsAllowedFromPureTo(defpur);
			}
			else return false;
		}
		else {
			return false;
		}
	}
	
	private static PurityStatus purityCheckAllFreeVars(Puretic pur, PurityContext pc) throws FisherException {
		Set<Id> freeIds = FreeIds.of((Syntax) pur);
		boolean canBeInnocent = true;
		boolean markedPure = pur.isMarkedPure();
		List<String> details = list();
		for (Id id : freeIds) {
			Seal seal = id.seal();
			if (PredefinedIdentifiers.PURE_PREDEFS.contains(id.str())) {
				// A built-in funtion known to be pure.
				continue;
			}
			if (id == pur.idOfName()) {
				// We're doing 'method m()=1;', and looking at 'm'.
				continue;
			}
			else if (pc.valSealsFromSurroundingClsDecl != null && pc.valSealsFromSurroundingClsDecl.contains(seal)) {
				// E.g., if we're doing 
				//   method m() = x+y; 
				// from 
				// class C{x = 1; y = 2; method m() = x+y;}
				// So these are OK.  
				continue;
			}
			else if (sealedWithPurity(seal, pur)) {
				// Looking good so far.
				continue;
			}
			else {
				canBeInnocent = false;
				details.add(id + " is neither pure nor innocent.");
			}
		}
		//OK, markedPure and innocent are now set. Do something with them
		if (canBeInnocent && markedPure) {
			return (PURE);
		}
		else if (canBeInnocent && !markedPure) {
			return (INNOCENT);
		}
		else if (!canBeInnocent && !markedPure) {
			return (IMPURE);
		}
		else if (!canBeInnocent && markedPure) {
			// But this is bad!  It lied about its purity status.  It must be stoned and punished!
			pur.flag(DangerLevel.ERROR, "Claimed to be pure, but it is impure!", sep(details, "\n"));
			return ERROR;
		}
		else {
			Doom.internal("Math is broken", (Syntax)pur);
			return ERROR;
		}
		
	}
	
	public static PurityStatus combine(Puretic pur,  PurityStatus... p) throws FisherException{
		PurityStatus cur = p[0];
		for (PurityStatus purityStatus : p) {
			cur=cur.and(purityStatus);
		}
		if (pur.isMarkedPure()) {
			switch(cur) {
			case PURE: return PURE;
			case INNOCENT: return PURE;
			case IMPURE :
				pur.flag(DangerLevel.ERROR, "This is impure, but marked pure", "");
				return ERROR;
			case ERROR : return ERROR;
			default: Doom.internal("Bad purity combination", (Syntax) pur); return null;
			}
		}
		else /* not marked pure */ {
			switch(cur) {
			case PURE: return INNOCENT;
			case INNOCENT: return INNOCENT;
			case IMPURE : return IMPURE;
			case ERROR : return ERROR;
			default: Doom.internal("Bad purity combination", (Syntax) pur); return null;
			}
		}
	}
	
	private  PurityStatus purityCheckSubexpressions(Puretic pur, PurityContext pc) throws FisherException {
		List<Syntax> descs = ((Syntax)pur).computeDescendants();
		PurityStatus p = INNOCENT;
		for (Syntax syntax : descs) {
			if (syntax instanceof Puretic && syntax != pur) {
				Puretic purkid = (Puretic) syntax;
				purkid.accept(this, pc);
				PurityStatus kidStatus = purkid.purityStatus();
				p = p.and(kidStatus);
			}
		}
		return p;
	}
	
	public static PurityStatus purityCheckForVarMembers(Classlike cls) throws FisherException {
		// The rule here is: no var members
		PurityStatus p = PURE;
		for (ClassMember mem : cls.members()) {
			p = p.and(mem.accept(MemberPurity.IT, MemberPurity.zonk));
		}
		return p;
	}
	
	public  PurityStatus purityCheckMethods(Classlike cls, PurityContext innerContext) throws FisherException {
		PurityStatus p = INNOCENT;
		for(ClassMember mem : cls.members()) {
			if (mem instanceof MethDecl) {
				MethDecl meth = (MethDecl) mem;
				meth.accept(this, innerContext);
				PurityStatus q = meth.purityStatus();
				p = p.and(q);
			}
		}
		return p;
	}

	public void visit(AnonObj syn, PurityContext arg) throws FisherException {
		ClsDecl generatedClsDecl = AnonObjDesugarer.retrieveGeneratedClsDecl(syn);
		checkInternal(generatedClsDecl);
		PurityStatus purityStatus = generatedClsDecl.purityStatus();
		syn.setPurityStatus(purityStatus);
	}
	
	public static Set<Seal> valFieldSeals(ClsDecl clsDecl) {
		ClassStatic cs = clsDecl.classStatic;
		Set<Seal> fieldSeals = cs.fieldSeals;
		Set<Seal> S = Bard.set();
		for (Seal seal : fieldSeals) {
			if (seal.kind == VAL) {
				S.add(seal);
			}
		}
//		for(ClassStatic parent : cs.superclasses) {
//			ClsDecl parentClsDecl = (ClsDecl) parent.clsDecl;
//			Set<Seal> parentValFields = valFieldSeals(parentClsDecl);
//			S.addAll(parentValFields);
//		}
		return S;
	}

	public void visit(ClsDecl syn, PurityContext arg) throws FisherException {
		PurityContext pc = new PurityContext(syn, valFieldSeals(syn));
		PurityStatus p1 = purityCheckAllFreeVars(syn, pc);
		PurityStatus p2 = purityCheckForVarMembers(syn);
		PurityStatus p3 = purityCheckMethods(syn, pc);
		PurityStatus p4 = purityCheckSubexpressions(syn, pc);
		syn.setPurityStatus(combine(syn,  p1,p2, p3, p4));
	}

	public void visit(FunBody syn, PurityContext pc) throws FisherException {
		PurityStatus p1 = purityCheckAllFreeVars(syn, pc);
		PurityStatus p2 = purityCheckSubexpressions(syn, pc);
		syn.setPurityStatus(combine(syn, p1, p2));
	}

	public void visit(MethDecl syn, PurityContext pc) throws FisherException {
		PurityStatus p1 = purityCheckAllFreeVars(syn, pc);
		PurityStatus p2 = purityCheckSubexpressions(syn, pc);
		PurityStatus combined = combine(syn, p1, p2);
		syn.setPurityStatus(combined);
	}

	public void visit(MonoBody syn, PurityContext pc) throws FisherException {
		PurityStatus p1 = purityCheckAllFreeVars(syn, pc);
		PurityStatus p2 = purityCheckSubexpressions(syn, pc);
		syn.setPurityStatus(combine(syn, p1, p2));
	}
	
	public void visit(FunDecl syn, PurityContext pc) throws FisherException {
		PurityStatus p1 = purityCheckAllFreeVars(syn, pc);
		PurityStatus p2 = purityCheckSubexpressions(syn, pc);
		syn.setPurityStatus(combine(syn, p1, p2));
	}
	
	public void visit(AnonFun syn, PurityContext pc) throws FisherException {
		PurityStatus p1 = purityCheckAllFreeVars(syn, pc);
		PurityStatus p2 = purityCheckSubexpressions(syn, pc);
		syn.setPurityStatus(combine(syn, p1, p2));
	}


}
