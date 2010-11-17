
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
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import fisher.eval.Evaller;
import fisher.eval.Frame;
import fisher.eval.ModuleDynamizer;
import fisher.runtime.ModuleDynamicTh;
import fisher.syn.ClsDecl;
import fisher.syn.Module;
import fisher.syn.core.Syntax;
import fisher.syn.interfaces.ModuleFileMember;
import fisher.util.Bard;
import fisher.util.DangerLevel;
import fisher.util.FisherException;
import fisher.util.SpecialCharacters;

public  class  ModuleStatic  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	private Map<String, Seal> provides = new HashMap<String, Seal>(); 
	
	public Collection<Seal> providedSeals() { 
		return this.provides.values();
	}
	
	
	
	public final Module src;
	private final String name;
	private SealForModule moduleSeal; // The seal for the module itself.
	
	public  final List<SealForClass> sealsOfClasses;

	public ModuleStatic(Module src) throws FisherException{
		super();
		this.src = src;
		this.moduleSeal = SealMaker.forModule(src, null, this); // NOTE-modules-cannot-be-nested
		ExtractSealsFromModuleMember sealExtractor = new ExtractSealsFromModuleMember(moduleSeal);
		this.provides = sealExtractor.extract(src);
		this.name = src.name.toString();
		this.sealsOfClasses = computeSealsOfClasses();
	}
	
	public String name() { 
		return this.name;
	}
	
	public String toString() {
		return SpecialCharacters.MODULE + this.name(); 
	}
	
	private List<SealForClass> computeSealsOfClasses() {
		List<SealForClass> S = Bard.list();
		for (ModuleFileMember bit : src.bits) {
			if (bit instanceof ClsDecl) {
				ClsDecl cd = (ClsDecl) bit;
				SealForClass s = cd.classSeal();
				S.add(s);
			}
		}
		return S;
	}
	
	public SealForModule moduleSeal() { return this.moduleSeal; }

	
	/**
	 * @param modules
	 * @return a list of ModuleStatics with their .provides() set right, but that's all.
	 * @throws FisherException
	 */
	public static List<ModuleStatic> bareStaticsFor(List<Module> modules) throws FisherException {
		List<ModuleStatic> L = new ArrayList<ModuleStatic>(modules.size());
		for (Module mod : modules) {
			L.add( new ModuleStatic(mod)); 
		}
		return L;
	}
	
	public Seal findSealForMember(String memberName, Syntax src) throws FisherException {
		if (provides.containsKey(memberName)) {
			return provides.get(memberName);
		}
		else {
			src.flag(DangerLevel.ERROR, "No definition for " + memberName + " in " + this.name(), "");
			return null;
		}
	}
	
	/**
	 * @param memberName
	 * @param src
	 * @return The seal of the member named <code>memberName</code>, or null.  
	 * No flagging of errors from this one.
	 * @throws FisherException
	 */
	public Seal findSealOrNull(String memberName)  {
		if (provides.containsKey(memberName)) {
			return provides.get(memberName);
		}
		else {
			return null;
		}
	}
	
	public ModuleDynamicTh instantiate(Evaller evaller, Syntax origSrc, Frame frame) throws FisherException {
		ModuleDynamicTh minth = new ModuleDynamicTh(this, frame);
		ModuleDynamizer.fillInAllFields(minth, evaller, origSrc);
		return minth;
	}
	
}
