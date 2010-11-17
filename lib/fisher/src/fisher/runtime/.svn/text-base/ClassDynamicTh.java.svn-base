
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

import java.io.IOException;
import java.io.ObjectStreamException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.Frame;
import fisher.eval.Matchiste;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.runtime.auxil.Typelike;
import fisher.statics.ClassStatic;
import fisher.statics.MethodSig;
import fisher.statics.Seal;
import fisher.statics.SealForClass;
import fisher.statics.purity.PurityStatus;
import fisher.syn.ClsCtorDef;
import fisher.syn.ClsDecl;
import fisher.syn.ClsExtends;
import fisher.syn.Cmd;
import fisher.syn.MonoBody;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.syn.interfaces.ClassMember;
import fisher.syn.interfaces.Classlike;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  ClassDynamicTh extends ThingBuiltIn implements Framelike, Applyable, Serializable, Typelike  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	
	
	public final Classlike clsDecl;
	public final ClassStatic classStatic;
	public final SealForClass seal;
	public final Framelike classFrame;
	public final List<ClassDynamicTh> superclassDynamics;
	private Set<ClassDynamicTh> ancestorClasses = null;
	


	public String toString() {
		return this.clsDecl.name().str();
	}

	public String className() {
		return this.clsDecl.name().str();
	}

	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		return true;
	}

	public ClassDynamicTh(Classlike clsDecl, Framelike frame) throws FisherException {
		super();
		this.clsDecl = clsDecl;
		this.classStatic = clsDecl.classStatic();
		assert (classStatic != null);
		this.seal = clsDecl.classSeal();
		this.classFrame = Frame.inner(frame);
		this.superclassDynamics = new ArrayList<ClassDynamicTh>(classStatic.superclasses.size());
		for (ClsExtends ext : clsDecl.extendses()) {
			Seal superclassSeal = ext.superclassSeal;
			Thing su = frame.RValue(superclassSeal, (Syntax) clsDecl);
			if (su instanceof ClassDynamicTh) {
				ClassDynamicTh cd = (ClassDynamicTh) su;
				superclassDynamics.add(cd);
			} else {
				Doom.runtime("Not a class: " + superclassSeal, (Syntax) clsDecl, su);
			}
		}
		this.addFramesOfSuperclassesToClassFrame();
	}

	private void addFramesOfSuperclassesToClassFrame() throws FisherException {
		Syntax src = (Syntax) clsDecl;
		for (ClassDynamicTh superclassD : this.superclassDynamics) {
			Framelike superFrame = superclassD.classFrame;
			List<Seal> superclassSeals = superFrame.seals();
			for (Seal seal : superclassSeals) {
				Frameable f = superFrame.baseValue(seal, src);
				if (classFrame.hasSeal(seal)) {
					Frameable alreadyStored = classFrame.baseValue(seal, src);
					if (alreadyStored == f)
						continue;
					Doom.internal("Collision importing superclass frames", src, seal, superclassD);
				} else {
					classFrame.store(seal, f, src);
				}
			}
		}
	}

	@Override
	public String typeString() {
		// TODO Auto-generated method stub
		return "class";
	}

	public Thing apply(Thing[] args, Framelike frame, Evaller evaller, Syntax src) throws FisherException {
		return construct(args, frame, evaller, src);
	}
	public Thing construct(Thing[] args, Framelike frame, Evaller evaller, Syntax src) throws FisherException {
		ObjectTh oth = ObjectTh.uninitialized(this, evaller);
		initializeFields(oth, args, frame, evaller, src);
		oth.setBeingConstructed(true);
		try {
			this.classStatic.runConstructorCode(oth, args, evaller, src);
		} finally {
			ifPureEnforcePurity(oth, src);
			oth.setBeingConstructed(false);
		}
		if (oth.hasMethod(MethodSig.INIT)) oth.invokeMethod(MethodSig.INIT.name, EvalUtil.NO_ARGS,   src);
		return oth;
	}

	private void ifPureEnforcePurity(ObjectTh oth, Syntax src) throws FisherException {
		if (classStatic.purityStatus() != PurityStatus.PURE)
			return;
		Map<String, Frameable> fieldsmap = oth.fields;
		for (Map.Entry<String, Frameable> ent : oth.fields.entrySet()) {
			Frameable fr = ent.getValue();
			if (EvalUtil.isPure(fr)) {
				// Yay!
			} else if (!(fr instanceof Thing)) {
				Doom.runtime("Violation of purity: field " + ent.getKey() + " has value " + fr
						+ " which is impure; it is not even a Thing.", src);
			} else {
				Doom.runtime("Violation of purity: field " + ent.getKey() + " has value " + fr + " of type "
						+ ((Thing) fr).typeString() + ", and is impure.", src);
			}
		}
	}

	public void initializeFields(ObjectTh oth, Thing[] args, Framelike frame, Evaller evaller, Syntax src)
			throws FisherException {
		for (ClassDynamicTh superCD : this.superclassDynamics) {
			// TODO -- Here's where we assume no args in class-header super call (or one such place).
			superCD.initializeFields(oth, EvalUtil.NO_ARGS, frame, evaller, src);
		}
		for (ClassMember initter : this.classStatic.instanceInitCode) {
			evaller.eval((Cmd) initter, oth);
		}
		// Constructor call...
	}

	public void invokeConstructorForSupercall(ObjectTh object, ClsCtorDef ccd, Thing[] args, Evaller evaller,
			Framelike frame, Syntax src) throws FisherException {
//		Frame innerFrame = Frame.inner(this);
		Frame innerFrame = Frame.inner(object);
		innerFrame.setThis(object);
		boolean matchFormals = Matchiste.matchFormals(ccd.monobody.formals, args, innerFrame);
		if (!matchFormals) {
			Doom.runtime("Formals don't match actuals in constructor call", ccd, src, object, frame);
		}
		evaller.eval(ccd.monobody.body, innerFrame);

	}

	public Frameable baseValue(Seal seal, Syntax src) throws FisherException {
		return classFrame.baseValue(seal, src);
	}

	public VarCell LValue(Seal seal, Syntax src) throws FisherException {
		return classFrame.LValue(seal, src);
	}

	public Thing RValue(Seal seal, Syntax src) throws FisherException {
		return classFrame.RValue(seal, src);
	}

	public void store(Seal seal, Frameable f, Syntax src) throws FisherException {
		classFrame.store(seal, f, src);
	}

	public Thing fetchImmediateMemberR(Seal seal, Syntax src) throws FisherException {
		return classFrame.fetchImmediateMemberR(seal, src);
	}

	public ObjectTh theThis(Evaller evaller, Syntax src) throws FisherException {
		if (this.classFrame != null)
			return this.classFrame.theThis(evaller, src);
		else
			return Doom.noThis(evaller, src);
	}

	public List<Seal> seals() {
		return classFrame.seals();
	}

	public boolean hasSeal(Seal seal) {
		return classFrame.hasSeal(seal);
	}

	public boolean hasInstance(Thing thing) {
		return thing instanceof ObjectTh ? this.hasInstance((ObjectTh) thing) : false;
	}
	public boolean hasInstance(ObjectTh object) {
		if (object == null)
			return false;
		ClassDynamicTh oclass = object.classDynamic;
		oclass.computeAncestorClassesIfNecessary();
		return oclass.ancestorClasses.contains(this);
	}

	private void computeAncestorClassesIfNecessary() {
		if (ancestorClasses != null)
			return;
		ancestorClasses = new HashSet<ClassDynamicTh>();
		for (ClassDynamicTh parent : this.superclassDynamics) {
			parent.computeAncestorClassesIfNecessary();
			this.ancestorClasses.addAll(parent.ancestorClasses);
		}
		this.ancestorClasses.add(this);
	}

	private final static int STR = 0;
	private final static int NUM = 1;
	private static Map<String, Integer> methodCode = new HashMap<String, Integer>();
	static {
		methodCode.put("str", STR);
		methodCode.put("num", NUM);
	}

	@Override
	public Thing invokeMethod(String methodName, Thing[] args,  Syntax src)
			throws FisherException {
		if (methodCode.containsKey(methodName)) {
			Integer methodC = methodCode.get(methodName);
			Evaller evaller = Evaller.mine();
			Framelike frame = this.frameForInvoke();
			switch (methodC) {
			case STR:
				return this.str();
			case NUM:
				return this.tryThingMethods(methodName, args,   src);
			default:
				Doom.internal("Method key structure broken", src, methodC);
				return null;
			}
		}
		return this.tryThingMethods(methodName, args,   src);
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

	
	private static final String header = "";
	private static final String tailer = "";
	
	private String classStaticKey; //used only for serialization + replaceObject. 
	
	// Serialization foo
	private void writeObject(java.io.ObjectOutputStream out)
    throws IOException {
		try {
			if (classStatic.purityStatus() != PurityStatus.PURE) {
				Doom.runtimeNonThorn("Impure object cannot be serialized!", Evaller.mine().lastSyntax(), 
						"ClassDynamic = " + this, "java class = " + this.getClass(),
						"Purity Status = " + classStatic.purityStatus());
			}
		} catch (FisherException e) {
			e.printStackTrace();
		} 
		// TODO -- this is none too efficient.  
		// When I am more confident about the code, I will write it directly to the ObjectOutputStream.
		StringBuffer sb = new StringBuffer();
		sb.append(header);
		sb.append(classStatic.serializationKey());
		sb.append(tailer);
		String sbs = sb.toString();
//		System.err.println("ClassDynamicTh.writeObject: " + sbs);
		out.writeObject(sbs);
	}

	
	// See diatribe on what's going on here in LetterWithSerializedContents.
	
	private void readObject(java.io.ObjectInputStream in) throws IOException, ClassNotFoundException {
		classStaticKey = (String) in.readObject();
//		System.err.println("ClassDynamic.readObject: reading " + classStaticKey);
		
		
	}
	
	Object readResolve() throws ObjectStreamException {
//		System.err.println("ClassDynamicTh.readResolve: wow, I want to resolve  " + classStaticKey);
//		System.err.println("This thread is of class " + Thread.currentThread().getClass());
		ClassDynamicTh cd = Evaller.mine().classDynamicForKey(classStaticKey);
		
		if (cd == null) {
			System.err.println("Oh dear oh dear oh dear");
			Map<String, ClassDynamicTh> loadedSharedClasses = Evaller.mine().loadedSharedClasses;
			System.err.println("I know these class keys:" + Bard.sep(loadedSharedClasses.keySet(), ", "));			
			Doom.runtimeNonThorn("Oh, dear.  No class matching " + classStaticKey + " is loaded here", Evaller.lastSyntax());
			
		}
//		System.err.println("CalssDyanmicTh.readResolve: wow, I resolved it to " + cd);
		return cd;
	}

}
