
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

import java.util.List;

import com.sun.org.apache.bcel.internal.generic.IfInstruction;

import fisher.eval.interfaces.Fieldiferous;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.AbstractRangeTh;
import fisher.runtime.BuiltInType;
import fisher.runtime.ClassDynamicTh;
import fisher.runtime.ExtensionClass;
import fisher.runtime.ListTh;
import fisher.runtime.ObjectTh;
import fisher.runtime.Thing;
import fisher.runtime.ThingExtended;
import fisher.statics.BuiltInTypeEnum;
import fisher.statics.ClassStatic;
import fisher.statics.Seal;
import fisher.statics.SealBuiltInType;
import fisher.syn.*;
import fisher.syn.core.Id;
import fisher.syn.visitor.VanillaVisitPat;
import fisher.util.Doom;
import fisher.util.FisherException;
import fisher.util.FisherRuntimeException;

public  class  Matchiste implements fisher.syn.visitor.VisitPat2<Thing, Framelike, Boolean, FisherException>  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	private static Matchiste MATCHISTE = new Matchiste();

	public static boolean matchFormals(Formals formals, Thing[] args, Frame frame) throws FisherException {
		List<Pat> pats = formals.formals;
		if (pats.size() != args.length)
			return false;
		for (int i = 0; i < args.length; i++) {
			Thing argi = args[i];
			Pat pati = pats.get(i);
			if (!match(pati, argi, frame)) {
				// TODO -- clean up frame, deleting newly-introduced bindings
				return false;
			}
		}
		return true;
	}

	private static Matchiste IT = new Matchiste();

	public static boolean match(Pat pat, Thing subject, Framelike frame) throws FisherException {
		return pat.accept(Matchiste.IT, subject, frame);
	}

	

	public Boolean visit(Pat syn, Thing subject, Framelike frame) throws FisherException {
		Doom.notYet();
		return false;
	}

	public Boolean visit(PatVar pat, Thing subject, Framelike frame) throws FisherException {
		Seal seal = pat.id.seal();
		frame.store(seal, subject, pat);
		return true;
	}

	public Boolean visit(PatLiteral pat, Thing subject, Framelike frame) throws FisherException {
		Thing plithing = pat.thing;
		return EvalUtil.eq(plithing, subject);
	}

	public Boolean visit(PatListCtor pat, Thing subject, Framelike frame) throws FisherException {
		PatListCtor ctor = (PatListCtor) pat;
		if (subject == null || !(subject instanceof ListTh)) return false;
//		if (simpleListCase(ctor)) {
//			final boolean b = matchSimpleListCase(ctor, subject, frame);
//			return b;
//		} else {
			final boolean b = matchComplexListCase(ctor, subject, frame);
			return b;
//		}
	}

	public Boolean visit(PatAnd pat, Thing subject, Framelike frame) throws FisherException {
		for (Pat subpat : pat.subpats) {
			boolean b = subpat.accept(this, subject, frame);
			if (!b) {
				// TODO -- undo bindings
				return false;
			}
		}
		return true;
	}

	public Boolean visit(PatExtract pat, Thing subject, Framelike frame) throws FisherException {
		// C(p,q) is interpreted as: 
		// 1. A type test subject:C
		// 2. If C is defined as class C(x,y) {...}, 
		// 3. Then match subject.x ~ p, subject.y ~ q
		// WAS: Seal typeSeal = pat.patname.seal();
		Thing typeThing = thingFromQualName(pat.patname, frame);
		if (! performTypeTest(pat, null, subject, frame, typeThing)) return false;
		// WAS: Thing typeThing = frame.RValue(typeSeal, pat);
		if (typeThing instanceof ClassDynamicTh) {
			ClassDynamicTh cls = (ClassDynamicTh) typeThing;
			ClassStatic cs = cls.classStatic;
			List<String> fieldNames = cs.fieldNamesOfDefaultConstructor;
			return checkTheSubpatterns(pat, subject, frame, fieldNames);
			
		}
		else if (typeThing instanceof ExtensionClass) {
			ExtensionClass ec = (ExtensionClass) typeThing;
			if (ec.implClass.isInstance(subject)) {
				List<String> fieldNames = ec.fieldNames;
				return checkTheSubpatterns(pat, subject, frame, fieldNames);
			}
			else {
				return false;
			}
		}
		else {
			Doom.runtime("Not a class or type", pat, "The supposed type actually is:", typeThing, "which is a " + typeThing.typeString());
			return false; // won't happen
		}
	}



	private Boolean checkTheSubpatterns(PatExtract pat, Thing subject, Framelike frame,
			List<String> fieldNames) throws FisherRuntimeException, FisherException {
		if (fieldNames == null){
			Doom.runtime("Class doesn't have a default constructor", pat,  subject);
		}
		int nFn = fieldNames.size();
		int nSp = pat.subpats.size();
		if (nFn != nSp) {
			Doom.runtime("Class has " + nFn + " fields to match, but there are " + nSp + " patterns in the expression " + pat, pat, subject );
		}
		if (subject instanceof Fieldiferous) {
			Fieldiferous fieldiferous = (Fieldiferous) subject;
			for(int i = 0; i < nFn; i++ ) {
				boolean b = matchField(pat, frame, fieldiferous, fieldNames.get(i), pat.subpats.get(i));
				if (!b) return false;
			}
			return true;
		}
		else {
			return false;
		}
	}
	
	

	public Boolean visit(PatTypeTest syn, Thing subject, Framelike frame) throws FisherException {
		Seal typeSeal = syn.shouldBe.last().seal();
		Thing typeThing = thingFromQualName(syn.shouldBe, frame);
		return performTypeTest(syn, syn.subpat, subject, frame, typeThing);
	}
	
	public static Thing thingFromQualName(QualName qn, Framelike frame) throws FisherException {
		final List<Id> ids = qn.ids;
		Seal seal0 = ids.get(0).seal();
		Object cursor = frame.RValue(seal0, qn);
		for(int i = 1; i < ids.size(); i++) {
			Seal seali = ids.get(i).seal();
			if (cursor instanceof Framelike) {
				Framelike framelikei = (Framelike) cursor;
				cursor = framelikei.RValue(seali, qn);
			}
			else {
				Doom.internal("What is " + cursor + " and why is it a " + cursor.getClass(), qn);
			}
		}
		return (Thing) cursor;
	}
	
	public Boolean visit(PatRange syn, Thing subject, Framelike frame) throws FisherException {
		if (! subject.isRange()) return false;
		AbstractRangeTh range = subject.asRange(syn);
		return match(syn.low, range.low(), frame ) && match(syn.high, range.high(), frame);
	}



	private static Thing typeFromSeal(Seal typeSeal, Framelike frame, Pat src) throws FisherException, FisherRuntimeException {
		if (frame.hasSeal(typeSeal)) {
			return frame.RValue(typeSeal, src);
		}
		else if (typeSeal.container != null ) {
			Thing cont = typeFromSeal(typeSeal.container, frame, src);
			return null;
		}
		else {
			Doom.internal("Can't get binding of seal " + typeSeal + " in " + frame, src, typeSeal, frame);
			return null;
		}
	}
	
	private Boolean performTypeTest(Pat syn, Pat subpatOrNull, Thing subject, Framelike frame, Thing typeThing)
			throws FisherException, FisherRuntimeException {
//		if (typeSeal instanceof SealBuiltInType) {
//			SealBuiltInType sealB = (SealBuiltInType) typeSeal;
//			BuiltInTypeEnum bit = sealB.builtInType;
//			if (bit.is(subject)) {
//				return subpatOrNull == null ? true : match(subpatOrNull, subject, frame);
//			}
//			else return false;
//		}
//		else 
		{
			// Thing typeThing = typeFromSeal(typeSeal, frame, syn);// frame.RValue(typeSeal, syn);
			if (typeThing instanceof ClassDynamicTh) {
				ClassDynamicTh cls = (ClassDynamicTh) typeThing;
				if (subject instanceof ObjectTh) {
					ObjectTh objuct = (ObjectTh) subject;
					if (cls.hasInstance(objuct)) {
						return subpatOrNull == null ? true : match(subpatOrNull, subject, frame);
					}
					else {
						// not a member 
						return false; 
					}
				}
				else {
					// subject is not an object
					return false;
				}
			}
			else if (typeThing instanceof ExtensionClass) {
				ExtensionClass ec = (ExtensionClass) typeThing;
				if (ec.implClass.isInstance(subject)) {
					return subpatOrNull == null ? true : match(subpatOrNull, subject, frame);
				}
				else {
					return false;
				}
			} else if (typeThing instanceof BuiltInType) {
				BuiltInType bit = (BuiltInType) typeThing;
				BuiltInTypeEnum bite = bit.type;
				if (bite.is(subject)) {
					return subpatOrNull == null ? true : match(subpatOrNull, subject, frame);
					}
					else return false;
				}
			else {
				// type isn't a class or a built-in
				Doom.runtime("Not a class or type", syn, "The supposed type actually is:", typeThing, "which is a " + typeThing.typeString());
				return false; // won't happen
			}
		}
	}
	
	public Boolean visit(PatEvalTestExp pat, Thing subject, Framelike frame) throws FisherException {
		Thing oldIt = subject;
		Thing thing;
		try {
			frame.setIt(subject, pat);
			thing = Evaller.mine().eval(pat.exp, frame);
		}
		finally {
			frame.setIt(oldIt, pat);
		}
		return thing.asBoolean(pat);
	}

	public Boolean visit(PatInterpolation pat, Thing subject, Framelike frame) throws FisherException {
		Thing thingToMatch = Evaller.mine().eval(pat.exp, frame);
		return EvalUtil.eq(thingToMatch, subject);
	}

	public Boolean visit(PatMatchSomethingElse pat, Thing subject, Framelike frame) throws FisherException {
		Thing oldIt = frame.getIt(pat);
		frame.setIt(subject, pat);
		Thing newSubject = null;
		try {
			newSubject = Evaller.mine().eval(pat.exp, frame);
		}
		finally {
			frame.setIt(oldIt, pat);
		}
		return match(pat.pat, newSubject, frame);
	}

	public Boolean visit(PatNot pat, Thing subject, Framelike frame) throws FisherException {
		boolean b = match(pat.subpat, subject, frame);
		return !b;
	}

	public Boolean visit(PatNotNull pat, Thing subject, Framelike frame) throws FisherException {
		if (subject == Thing.NULL)
			return false;
		return match(pat.subpat, subject.deplus(), frame);
	}

	public Boolean visit(PatOr pat, Thing subject, Framelike frame) throws FisherException {
		for (Pat p : pat.subpats) {
			if(match(p, subject, frame)) return true;
		}
		return false;
	}

	
	private boolean matchField(Pat src, Framelike frame, Fieldiferous subject, String fieldName, Pat fieldShouldMatch) throws FisherException {
		if (subject.hasField(fieldName)) {
			Thing thatField = subject.getField(fieldName, src);
			return match(fieldShouldMatch, thatField, frame);
		}
		else {
			return false;
		}
	}
	
	// ‹ a:p1, b:p2, c:p3 ›
	public Boolean visit(PatRecordCtor pat, Thing subject, Framelike frame) throws FisherException {
		/*
		 * This method is one of the places where "Record is a special case of Object" is encoded.
		 */
		if (subject instanceof Fieldiferous) {
			Fieldiferous osubject = (Fieldiferous) subject;
			List<PatRecordField> fields = pat.fields;
			for (PatRecordField patRecordField : fields) {
				String fieldName = patRecordField.id.str();
				if (! matchField(pat, frame, osubject, fieldName, patRecordField.pat)) return false;
//				if (osubject.hasField(fieldName)) {
//					Thing thatField = osubject.getField(fieldName,  pat);
//					boolean b = match(patRecordField.pat, thatField, frame);
//					if (!b) return false;
//				}
//				else {
//					// No such field, so it doesn't match
//					return false;
//				}
			}
			// After the for-loop, we found all the fields.
			return true;
		}
		else {
			// not even an object
			return false;
		}
	}

	public Boolean visit(PatWildcard pat, Thing subject, Framelike frame) throws FisherException {
		return true;
	}

	public static boolean matchComplexListCase(PatListCtor ctor, Thing subject, Framelike frame) throws FisherException {
		// assert(! simpleListCase(ctor));
		ListTh L = subject.asList(ctor);
		return matchComplexListCaseInternal(ctor, L, frame, 0);
	}

	private static boolean matchComplexListCaseInternal(PatListCtor ctor, ListTh L, Framelike frame, int whichListBit) throws FisherException {
		if (whichListBit >= ctor.bits.size()) {
			return (L.length == 0);
		} else {
			final PatListBit patListBit = ctor.bits.get(whichListBit);
			if (patListBit instanceof PatListBitExp) {
				// Single element.
				if (L.isEmpty()) return false; // A single element will never match an empty list.
				PatListBitExp plbe = (PatListBitExp) patListBit;
				Pat pat = plbe.pat;
				Thing Li = L.car();
				final boolean matchThis = match(pat, Li, frame);
				if (matchThis) {
					if (L.isEmpty()) return matchComplexListCaseInternal(ctor, L, frame, whichListBit+1);
					final boolean matchRest = matchComplexListCaseInternal(ctor, L.cdr(), frame, whichListBit+1);
					return matchRest;
				} else return false;
			}
			else if (patListBit instanceof PatListBitEllip) {
				PatListBitEllip plbellip = (PatListBitEllip) patListBit;
				Pat pat = plbellip.pat;
				ListTh Lij = ListTh.EMPTY;
				ListTh cursor = L;
				for(int j = 0; j <= L.length; j++) {
					// Try by matching each element of L(i..(j-1)) against pat, and then match the rest of the list against the rest of ctor
					// At this point, we have tested elements i..(j-1), and they all match pat.
					doCaptureBit(pat, frame, Lij);
					if (matchComplexListCaseInternal(ctor, cursor, frame, whichListBit+1)) {
						return true;
					}
					// OK, that didn't work. Grow the sublist by one, and try that.
					if (j == L.length) {
						// Can't grow by one.
						return false;
					}
					Thing Lj = cursor.car();
					cursor = cursor.cdr(); 
					if (!match(pat, Lj, frame)) {
						return false;
					}
					Lij = Lij.followedBy(Lj);
				}
				return false;
			}
			else {
				Doom.internal("New kind of pattern component", ctor);
				return false;
			}
		}
	}

	private static void doCaptureBit(Pat pat, Framelike frame, ListTh Lij) throws FisherException {
		if (pat instanceof PatWildcard) {
			PatWildcard wildcard = (PatWildcard) pat;
			// no capture
		}
		else if (pat instanceof PatVar) {
			PatVar patvar = (PatVar) pat;
			frame.store(patvar.id.seal(), Lij, pat);
		}
		else {
			PatVar pv = shouldStoreSublist(pat);
			if (pv != null) frame.store(pv.id.seal(), Lij, pat);
		}
	}
	
	public static boolean matchSimpleListCase(PatListCtor ctor, Thing subject, Framelike frame) throws FisherException {
		assert (simpleListCase(ctor));

		if (subject == null) return false;
		
		if (!(subject.isList()))
			return false;
		ListTh L = subject.asList(ctor);

		List<PatListBit> bits = ctor.bits;
		int size = bits.size();

		// special case size=0 to avoid trouble testing last element

		if (size == 0) {
			return L.length == 0;
		}

		boolean lastIsEllipsis = bits.get(size - 1) instanceof PatListBitEllip;

		// size check
		if (lastIsEllipsis) {
			if (L.length < size - 1)
				return false;
		} else {
			if (L.length != size)
				return false;
		}

		for (int i = 0; i < size - 1; i++) {
			PatListBit bit = bits.get(i);
			Thing thingi = L.sub(i, ctor);
			if (bit instanceof PatListBitExp) {
				PatListBitExp pexp = (PatListBitExp) bit;
				boolean b = match(pexp.pat, thingi, frame);
				if (!b)
					return false;
			}
		}
		// last one can be x or x... --
		// This code parallels doCaptureBit (and should probably be replaced by a call to it plus other stuff)
		PatListBit lastbit = bits.get(size - 1);
		if (lastbit instanceof PatListBitExp) {
			Thing lastThing = L.sub(size - 1, ctor);
			PatListBitExp pexp = (PatListBitExp) lastbit;
			return match(pexp.pat, lastThing, frame);
		} else if (lastbit instanceof PatListBitEllip) {
			PatListBitEllip ellip = (PatListBitEllip) lastbit;
			if (ellip.pat instanceof PatVar) {
				PatVar pv = (PatVar) ellip.pat;
				ListTh tail = L.nthCdr(size - 1, lastbit);
				frame.store(pv.id.seal(), tail, lastbit);
				return true;
			}
			else if (ellip.pat instanceof PatWildcard) {
				PatWildcard patwi = (PatWildcard) ellip.pat;
				// _... at the end of the list always matches and never binds anything.
				return true;
			}
			else {
				for(int i = size; i < L.length; i++) {
					Thing thingi = L.sub(i, ctor);
					if (! match(ellip.pat, thingi, frame)) return false;
				}
				PatVar varToStoreInOrNull = shouldStoreSublist(ellip.pat);
				ListTh tail = L.nthCdr(size - 1, lastbit);
				if (varToStoreInOrNull != null) frame.store(varToStoreInOrNull.id.seal(), tail, ellip);
				return true;
//				Doom.notYet("I cant' do that, Dave");
//				return false;
			}
		} else {
			Doom.internal("Odd kind of list bit here, is " + lastbit + " of class " + lastbit.getClass(), lastbit,
					subject, frame);
			return false;
		}
	}

	private static PatVar shouldStoreSublist(Pat ellipPat) {
		if (ellipPat instanceof PatAnd) {
			PatAnd pand = (PatAnd) ellipPat;
			Pat pandl = pand.subpats.get(0);
			if (pandl instanceof PatVar) {
				PatVar pv = (PatVar) pandl;
				return pv;
			}
		}
		return null;
	}
	
	// [p1, p2, p3] or [p1, p2, x...] (with x an identifier)
	public static boolean simpleListCase(PatListCtor ctor) throws FisherException {
		List<PatListBit> bits = ctor.bits;
		int size = bits.size();
		for (int i = 0; i < size - 1; i++) {
			PatListBit bit = bits.get(i);
			if (!(bit instanceof PatListBitExp))
				return false;
		}
		// last one can be P or x...
		if (size == 0) {
			return true;
		}
		PatListBit lastBit = bits.get(size - 1);
		if (lastBit instanceof PatListBitExp) {
			return true;
		} else if (lastBit instanceof PatListBitEllip) {
			PatListBitEllip ellip = (PatListBitEllip) lastBit;
			return ellip.pat instanceof PatVar;
		} else {
			Doom.internal("WTF is " + lastBit, ctor);
			return false;
		}
	}
	
	private static Thing[] NO_ARGS = new Thing[0];
	
	public Boolean visit(PatMethodCall syn, Thing subject, Framelike frame) throws FisherException {
		if (subject == null) return false;
		Thing retted = subject.invokeMethod(syn.methodName, NO_ARGS,   syn);
		if (syn.subpat == null) {
			if (retted == null) return false;
			if (retted.isBoolean()) return retted.asBoolean(syn);
			return true;
		}
		else {
			if (retted == null) return false;
			return Matchiste.match(syn.subpat, retted, frame);
		}
	}


	public Boolean visit(PatSlash syn, Thing subject, Framelike frame) throws FisherException {
		return match(syn.actualCode, subject, frame);
	}
	
}
