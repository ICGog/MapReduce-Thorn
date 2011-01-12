
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.runtime;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.Frame;
import fisher.eval.interfaces.Fieldiferous;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.auxil.FisherReturn;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.statics.MethodSig;
import fisher.statics.Seal;
import fisher.syn.Cmd;
import fisher.syn.FunBody;
import fisher.syn.MethDecl;
import fisher.syn.MethodCall;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;
import fisher.util.SpecialCharacters;

public  class  ObjectTh extends Thing implements Framelike, Fieldiferous, Serializable  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public final ClassDynamicTh classDynamic;
	public final Map<String, Frameable> fields = new HashMap<String, Frameable>();
	private boolean beingConstructed = true;

	public static ObjectTh uninitialized(ClassDynamicTh classDynamic, Evaller evaller) {
		return new ObjectTh(classDynamic, evaller);
	}

	public static MethodSig toStr0 = new MethodSig("str", 0);

	public String toString() {
		if (!this.hasMethod(toStr0) || this.beingConstructed) {
			return SpecialCharacters.INSTANCE + this.classDynamic.classStatic.classSeal.str();
		}
		try {
			Thing t = invokeMethod("str", EvalUtil.NO_ARGS,  Evaller.mine().wholeSrc);
			return t.asString(null);
		} catch (FisherException e) {
			return SpecialCharacters.INSTANCE + this.classDynamic.classStatic.classSeal.str();
		}
	}

	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		try {
			return classDynamic.classStatic.purityStatus().canBeInPureObject();
		} catch (FisherException e) {
			Doom.internalCatastrophe(
					"Error computing purity status of something that should have its purity status just set", Evaller
							.lastSyntax());
			return false;
		}
	}
	
	@Override
	public boolean transmissible() {
		if (!super.transmissible()) return false;
		for (Frameable fram : fields.values()) {
			if (fram == null) continue;
			final Thing val = fram.Rvalue();
			if (val == null) continue;
			if (!val.transmissible()) return false;
		}
		return true;
	}

	private ObjectTh(ClassDynamicTh classDynamic, Evaller evaller) {
		super();
		this.classDynamic = classDynamic;
	}
	
	public Framelike frameForInvoke() {
		return this;
	}

	public Frameable baseValue(Seal seal, Syntax src) throws FisherException {
		String key = seal.str();
		if (fields.containsKey(key)) {
			return fields.get(key);
		} else {
			try {
				return this.classDynamic.classFrame.baseValue(seal, src);
			} catch (FisherException fe) {
				Doom.runtime("Object does not have a field sealed with " + seal, src, this);
				return null;
			}
		}
	}

	public Thing getField(String fieldName, Syntax src) throws FisherException {
		// Keep this in synch with 'hasField'! 
		if (fields.containsKey(fieldName)) {
			Frameable f = fields.get(fieldName);
			return f == null ? null : f.Rvalue();
		} else {
			Doom.runtime("Object does not have a field named " + fieldName, src, this);
			return null;
		}
	}

	public boolean hasField(String fieldName) {
		// Keep this in synch with 'getField!'
		if (fields.containsKey(fieldName)) {
			return true;
		} else {
			return false;
		}
	}

	public boolean hasMethod(MethodSig sig) {
		MethDecl methDecl = classDynamic.classStatic.findMethodOrNull(sig);
		return methDecl != null;
	}

	public static final MethodSig eqSig = new MethodSig("==", 1);
	public static final MethodSig hashCodeSig = new MethodSig("hashCode", 0);

	@Override
	public boolean canBeKey() {
		return hasMethod(eqSig) && hasMethod(hashCodeSig);
	}

	public VarCell LValue(Seal seal, Syntax src) throws FisherException {
		// TODO Auto-generated method stub
		Frameable bv = baseValue(seal, src);
		if (bv instanceof VarCell) {
			VarCell varCell = (VarCell) bv;
			return varCell;
		} else {
			Doom.runtime("Field " + seal.str() + " is not assignable.", src, this);
			return null;
		}
	}

	public Thing RValue(Seal seal, Syntax src) throws FisherException {
		Frameable bv = baseValue(seal, src);
		return bv == null ? null : bv.Rvalue();
	}

	public void storeIntoVarField(Seal uselessFieldSeal, Frameable f, Syntax src) throws FisherException {
		String str = uselessFieldSeal.str();
		String methodName = str + ":=";
		this.invokeMethod(methodName, Bard.array(f == null ? null : f.Rvalue()),   src);
	}

	public void OLD_storeIntoVarField(Seal uselessFieldSeal, Frameable f, Syntax src) throws FisherException {
		String str = uselessFieldSeal.str();
		if (this.classDynamic.classStatic.fieldNames.contains(str)) {
			if (fields.containsKey(str)) {
				// Check types.
				//				Seal fieldSealFromClass = this.classDynamic.classStatic.getSealForField(str);
				//				assert(fieldSealFromClass != null);
				//				fieldSealFromClass.check(this, f, src);
				// Do the store
				Frameable old = fields.get(str);
				if (old instanceof VarCell) {
					VarCell varCell = ((VarCell) old);
					if (f == null) {
						varCell.setCargo(null, src);
					} else if (f instanceof Thing) {
						Thing thing = (Thing) f;
						varCell.setCargo(thing, src);
					} else {
						Doom.internal("I can't store this!", src, f, uselessFieldSeal, this, old);
					}
				} else {
					Doom.internal("Trying to store into a var field, but it's not a var field", src, uselessFieldSeal,
							this, f);
				}
			} else {
				Doom.internal("fields doesn't containKey str", src, fields, str);
			}
		} else {
			Doom.internal("Attempt to store field " + str + " of an object which doesn't have that field", src);
		}
	}

	public void store(Seal seal, Frameable f, Syntax src) throws FisherException {
		seal.check(this, f, src);
		String str = seal.str();
		if (this.classDynamic.classStatic.fieldNames.contains(str)) {
			if (fields.containsKey(str)) {
				Frameable old = fields.get(str);
				if (old instanceof VarCell) {
					if (this.beingConstructed) {
						// Skip it -- it's a diamond (I think).
					} else {
						Doom.internal("I seem to be replacing a var cell in an object.", src, seal, f, this);
					}
				} else {
					fields.put(str, f);
				}
			} else {
				fields.put(str, f);
			}
		} else {
			Doom.runtime("Attempt to store field " + str + " of an object which doesn't have that field", src);
		}
	}

	public Thing fetchImmediateMemberR(Seal seal, Syntax src) throws FisherException {
		String key = seal.str();
		if (fields.containsKey(key)) {
			return fields.get(key).Rvalue();
		} else {
			Doom.runtime("Object does not have a field named " + key, src, this);
			return null;
		}
	}

	public boolean hasSeal(Seal seal) {
		if (this.sealShouldBeMine(seal)) {
			return fields.containsKey(seal.str());
		} else {
			return this.classDynamic.classFrame.hasSeal(seal);
		}
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null)
			return false;
		else if (obj instanceof ObjectTh) {
			ObjectTh objth = (ObjectTh) obj;
			Evaller evaller = Evaller.mine();
			Frame frame = Frame.inner(classDynamic.classFrame);
			Syntax src = Doom.maybeAt;

			try {
				Thing[] things = new Thing[] { objth };
				Thing th = invokeMethod("==", things,   src);
				return th.asBoolean(src);
			} catch (FisherException e) {
				Doom.guessedSource(e, "Failure in computing equality", this, obj);
				return false;
			}
		} else {
			return false;
		}
	}

	@Override
	public int hashCode() {
		Evaller evaller = Evaller.mine();
		Frame frame = Frame.inner(classDynamic.classFrame);
		Syntax src = Doom.maybeAt;
		try {
			Thing th = invokeMethod("hashCode", EvalUtil.NO_ARGS,   src);
			return (int) th.asLong(src);
		} catch (FisherException e) {
			Doom.guessedSource(e, "Failure in computing hash code", this);
			return 0;
		}
	}

	public boolean sealShouldBeMine(Seal seal) {
		return seal.container == this.classDynamic.seal;
	}

	public List<Seal> seals() {
		Doom.internalCatastrophe("Objects (considered as Frames) don't exactly have seals.", null);
		return null;
	}

	public Thing invokeMethod(String methodName, Thing[] args,  Syntax src)
			throws FisherException {
		MethodSig sig;
		// Warning: src may be a MethodCall, but, even if it is, it may not be the 
		// call to *this* method.  e.g., tbl.ins(row) -- tbl.ins calls 
		// hashCode(), internally, even though there's no visible call to 
		// hashCode.

		sig = new MethodSig(methodName, args.length);

		MethDecl methDecl = classDynamic.classStatic.findMethodOrNull(sig);
		if (methDecl == null) {
			// Methods that apply to all objects (but can be overridden) go here:
			if (methodName.equals(Thing.INVOKE)) {
				return this.do_invoke(args,  src);
			}
			
			return Doom.runtime("No method named " + methodName, src, this, args );
		}
		Thing res = invokeMethodInternal(args,  src, methDecl.funbody);
		return res;
	}
	

	public Thing invokeMethDecl(MethDecl methDecl, Thing[] args,  Syntax src)
			throws FisherException {
		Thing res = invokeMethodInternal(args,   src, methDecl.funbody);
		return res;
	}

	private Thing invokeMethodInternal(Thing[] args,  Syntax src, FunBody funbody)
			throws FisherException {
		Frame innerFrame = Frame.inner(this);
		innerFrame.setThis(this);
		Evaller evaller = Evaller.mine();
		Cmd body = EvalUtil.findAndBind(funbody, args, evaller, innerFrame, src); // note: innerFrame mutated
		try {
			Thing res = evaller.eval(body, innerFrame);
			return res;
			}
		catch (FisherReturn fr) {
			return fr.thing;
		}

	}

	@Override
	public String typeString() {
		return "object(" + this.classDynamic.className() + ")";
	}

	public ObjectTh theThis(Evaller evaller, Syntax src) throws FisherException {
		return this;
	}

	public boolean isBeingConstructed() {
		return beingConstructed;
	}

	public void setBeingConstructed(boolean beingConstructed) {
		this.beingConstructed = beingConstructed;
	}

	//	public Internal_Success invokePat(Id patId, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	//	throws FisherException {
	//		return this.patNotUnderstood(patId, args, evaller, frame, src);
	//	}

	private Thing it = null;

	public void setIt(Thing subject, Syntax src) throws FisherException {
		it = subject;
	}

	public Thing getIt(Syntax src) throws FisherException {
		return it;
	}
	
	public boolean sameTypeAndFieldsAs(ObjectTh b) throws FisherException {
		if (b == null) return false;
		if (this.classDynamic != b.classDynamic) return false;
		for (Map.Entry<String, Frameable> ent : this.fields.entrySet()) {
			String fieldName = ent.getKey();
			Thing thisValue = ent.getValue().Rvalue();
			Thing otherValue = b.fields.get(fieldName).Rvalue(); // Note: by class invariant, it exists.
			if (!EvalUtil.eq(thisValue, otherValue)) return false;			
		}
		return true;
	}

}
