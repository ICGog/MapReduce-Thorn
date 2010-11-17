
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

import fisher.syn.ImportStmt;
import fisher.syn.QualName;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.syn.importage.ImportModuleStar;
import fisher.util.Bard;
import fisher.util.DangerLevel;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  Env  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public static long UID = 0;
	public long uid = (UID++);
	private Env parent;
	public final Seal container;
	
	private boolean insidePattern;
	
	private final boolean doingClass ;
	private boolean doingNew = false;
	
	private  ClassStatic classStatic;

	private Map<String, Seal> localBindings = new HashMap<String, Seal>();

	/**
	 * All modules defined in the current environment, available for importing
	 * or what have you.
	 */
	private Map<String, ModuleStatic> availableModules;

	/**
	 * The list of all modules which are imported as .* (From this or an
	 * ancestor env)
	 */
//	private Set<ModuleStatic> importStarredModules;
	private Set<Seal> importStarredModules;

	private Map<Seal, ImportStmt> importStatementsOfStarredModules;

	private Env(Env parent, List<Seal> importedModules, List<ModuleStatic> modules,
			Map<String, ModuleStatic> availableModules, Seal container, Map<Seal, ImportStmt> isosm, 
			boolean insidePattern, boolean insideClass, ClassStatic classStatic) {
		this.parent = parent;
		this.availableModules = availableModules == null ? new HashMap<String, ModuleStatic>(modules.size())
				: availableModules;
		for (ModuleStatic moduleStatic : modules) {
			this.availableModules.put(moduleStatic.name(), moduleStatic);
		}
		this.importStarredModules = new HashSet<Seal>(importedModules.size()
				+ (parent == null ? 0 : parent.importStarredModules.size()));
		this.importStarredModules.addAll(importedModules);
		if (this.parent != null)
			this.importStarredModules.addAll(parent.importStarredModules);
		this.container = container;
		this.importStatementsOfStarredModules = isosm;
		this.insidePattern = insidePattern;
		this.doingClass = insideClass;
		this.classStatic = classStatic;
	}

	public static Env root(List<ModuleStatic> availableModules) {
		Env env =  new Env(null, Collections.EMPTY_LIST, availableModules, null, null,
				new HashMap<Seal, ImportStmt>(1), false, false, null);
		for (PredefinedIdentifiers.Predef predef : PredefinedIdentifiers.predefs) {
			try {
				env.define(predef.name, predef.seal);
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		}
		return env;
	}

	public Env inner() {
		return new Env(this, Collections.EMPTY_LIST, Collections.EMPTY_LIST, this.availableModules, this.container,
				copyOfMyImportStars(), this.insidePattern, this.doingClass, this.classStatic);
	}

	public Env innerWithNoImportStars() {
		return new Env(this, Collections.EMPTY_LIST, Collections.EMPTY_LIST, this.availableModules, this.container,
				new HashMap<Seal, ImportStmt>(), this.insidePattern, this.doingClass, this.classStatic);
	}
	
	public Env innerWithNoContainer() {
		return new Env(this, Collections.EMPTY_LIST, Collections.EMPTY_LIST, this.availableModules, null, 
				copyOfMyImportStars(), this.insidePattern, this.doingClass, this.classStatic);
	}

	public Env innerWithNewContainer(Seal container) {
		ClassStatic itsClassStatic;
		if(container instanceof SealForClass) {
			itsClassStatic = ((SealForClass) container).classStatic();
		}
		else {
			itsClassStatic = this.classStatic;
		}
		Env e = new Env(this, Collections.EMPTY_LIST, Collections.EMPTY_LIST, this.availableModules, container,
				copyOfMyImportStars(), this.insidePattern, this.doingClass || (container instanceof SealForClass), 
				itsClassStatic
		);
		
		return e;
	}

	public Env innerInsidePattern() {
		return new Env(this, Collections.EMPTY_LIST, Collections.EMPTY_LIST, this.availableModules, this.container,
				copyOfMyImportStars(), true, this.doingClass, this.classStatic);
	}
	
	/**
	 * Don't use this routinely -- it's for the REPL.
	 * @param ms
	 */
	public void addModule(ModuleStatic ms) {
		this.availableModules.put(ms.name(), ms);
	}

	
	public Env innerNew() {
		Env e = this.inner();
		e.doingNew = true;
		return e;
	}
	
	
	private HashMap<Seal, ImportStmt> copyOfMyImportStars() {
		return new HashMap<Seal, ImportStmt>(this.importStatementsOfStarredModules);
	}

	List<String> nonPredefBindings() {
		List<String> L = Bard.list();
		for (Map.Entry<String, Seal>ss  : this.localBindings.entrySet()) {
			if (! ss.getValue().builtIn) {
				L.add(ss.getKey());
			}
		}
		return L;
	}
	
	
	public String toString() {
		return "€" + uid + "{" + Bard.sep(nonPredefBindings(), ",") + "}"
		+ (parent != null ? parent.toString() : "")
		;
	}

	
	
	public Seal seal(String idstr, Syntax src) throws FisherException {
		if (this.hasSealFor_1(idstr)) {
			return this.seal_1(idstr, src);
		} else 
			if (this.hasOneOrMoreImportStarDefsFor(idstr)) {
			// A slightly subtle point here.
			// seal_2_caching produces an error message if we refer
			// to a colliding-imported identifier
			// (as in: module A{var x;} module B{var x;} {import A.*; import
			// B.*; x := 3; }
			// But the is-it-there-at-all function doesn't and shouldn't produce
			// error messages. So we look for one or more defs,
			// and, if there are one or more, seal_2_caching finds a unique one.
			// Oh, it also caches the seal locally, so we don't need to do that
			// rigamarole twice.
			return this.seal_2_caching(idstr, src);
		} else {
			src.flag(DangerLevel.ERROR, idstr + " is not defined [1]", "");
			return null;
		}
	}
	
	public Seal seal(QualName qn, Syntax src) throws FisherException {
		if (qn.ids.isEmpty()) {
			Doom.internal("Empty qualified name?", src);
			return null;
		}
		else if (qn.ids.size() == 1){
			Id id = qn.ids.get(0);
			return this.seal(id, src);
		}
		else {
			Doom.notYet();
			return null;
		}
	}

	public Seal seal_1(String idstr, Syntax src) throws FisherException {
		if (localBindings.containsKey(idstr))
			return localBindings.get(idstr);
		else if (this.parent == null) {
			src.flag(DangerLevel.ERROR, idstr + " is not defined. [2]", "");
			return null;
		} else {
			return this.parent.seal(idstr, src);
		}
	}

	public boolean hasSealFor(String idstr) {
		boolean hasSealFor_1 = hasSealFor_1(idstr);
		boolean b = hasSealFor_1 || hasSealFor_2(idstr);
		return b;
	}

	public boolean hasSealFor(Id id) {
		return this.hasSealFor(id.str());
	}

	public boolean hasSealFor(QualName qn)  {
		if (qn.ids.isEmpty()) {
			throw new RuntimeException("Internal compiler doom in a nasty place.");
		}
		else if (qn.ids.size() == 1){
			Id id = qn.ids.get(0);
			return this.hasSealFor(id);
		}
		else {
			Doom.notYet();
			return false;
		}
	}
	
	public String mostNamesBound() {
		Env e = this;
		List<String> L = new ArrayList<String>();
		while(e.parent != null) {
			L.addAll(e.localBindings.keySet());
			e = e.parent;
		}
		Collections.sort(L);
		return Bard.sep(L,",");
	}
	
	private boolean hasSealFor_1(String idstr) {
		if (localBindings.containsKey(idstr))
			return true;
		else if (this.parent == null) {
			return false;
		} else {
			boolean b = this.parent.hasSealFor_1(idstr);
			return b;
		}
	}

	private boolean hasOneOrMoreImportStarDefsFor(String idstr) {
		{
			for (Seal modSeal : this.importStarredModules) {
				assert (modSeal.kind == SealKind.MODULE);
				
				Seal sealInModule = modSeal.sealOfContained(idstr);
				if (sealInModule != null) {
					return true;
				}
			}
			return false;
		}
	}

	private boolean hasSealFor_2(String idstr) {
		int nFound = 0;
		Seal foundSeal = null;
		for (Seal modSeal : this.importStarredModules) {
			/*
			// The following is wrong, because "sealOfContained" (and, in general, seal contents and containers) are 
			// kind of screwed up.
			Seal sealInModule = modSeal.sealOfContained(idstr);
			if (sealInModule != null) {
				foundSeal = sealInModule;
				nFound += 1;
			}
			*/
			ModuleStatic ms = ((SealForModule)modSeal).moduleStatic;
			Seal sealInModule = ms.findSealOrNull(idstr);
			if (sealInModule != null) {
				foundSeal = sealInModule;
				nFound += 1;
			}
		}
		if (nFound == 1) {
			return true;
		} else {
			return false;
		}
	}

	private Seal seal_2_caching(String idstr, Syntax src) throws FisherException {
		Set<Seal> modulesDefiningIt = new HashSet<Seal>(0);
		Seal foundSeal = null;
		for (Seal modSeal : this.importStarredModules) {
			Seal sealInModule = modSeal.sealOfContained(idstr);
			if (sealInModule != null) {
				foundSeal = sealInModule;
				modulesDefiningIt.add(modSeal);
			}
		}
		
		
		if (modulesDefiningIt.size() == 1) {
			// Make a local alias so we can mess with the alias.
			foundSeal = foundSeal.alias(foundSeal.str());  // HOPE HOPE HOPE
			
			// Cache it...
			this.localBindings.put(idstr, foundSeal);
			// Annotate the 'import M.*' stmt that imported it, explaining that
			// we have to import idstr.
			// ModuleStatic ms = Bard.someElementOf(modulesDefiningIt);
			Seal definingModSeal = Bard.someElementOf(modulesDefiningIt);
			ImportStmt imp = this.getImportStatementFor(definingModSeal, src);
			foundSeal.setImportStmt(imp); // HOPE HOPE HOPE
			if (!imp.importage.modulesToImport.contains(definingModSeal)) {
				imp.importage.modulesToImport.add(definingModSeal);
			}
			if (imp.importage instanceof ImportModuleStar) {
				ImportModuleStar ims = (ImportModuleStar) imp.importage;
				ims.addMemberToImport(foundSeal);
			} else {
				Doom.internal("Something that should have been an ImportModuleStar, wasn't", src, imp, imp.importage);
			}

			boolean b = foundSeal.container == definingModSeal;
			if (!b) {
				// I wanna set a breakpoint here.
				assert (b);
			}
			return foundSeal;
		} else {
			src.flag(DangerLevel.ERROR, "More than one *-imported module defines " + idstr, "The modules "
					+ Bard.sep(modulesDefiningIt, ",") + " all define it.");
			return null;
		}
	}

	public Seal seal(Id id, Syntax src) throws FisherException {
		return seal(id.str(), src);
	}

	public void define(String str, Seal seal) throws FisherException {
		// String str = seal.str();
		boolean hasSealFor = this.hasSealFor(str);
		boolean b = hasSealFor && this.seal(str, null) != seal;
		if (b) {
			duplicateDefinitionDoom(str, seal, this);
		}
		this.localBindings.put(str, seal);
	}

	public static void duplicateDefinitionDoom(String str, Seal seal, Env env) throws FisherException {
		Syntax def1 = env.seal(str, null).def;
		if (def1 == null) {
			seal.def.flag(DangerLevel.ERROR, "Dublicate definition", "Attempt to redefine the built-in identifier " + str					);
		}
		else seal.def.flag(DangerLevel.ERROR, "Duplicate definition", "[1]There are two nested definitions for identifier "
				+ str , "seal.def =  " + seal.def + " ---  " + seal.def.original(), "\ndef from env= " + def1 + " --- " + def1.original());
	}

	public void noteImportStar(Seal moduleSeal, ImportStmt is) throws FisherException {
		assert(moduleSeal.kind == SealKind.MODULE);
		if (importStatementsOfStarredModules.containsKey(moduleSeal)) {
			is.flag(DangerLevel.WARNING, "Multiple import of module " + moduleSeal.str(), "", "\nfirst one: ",
					importStatementsOfStarredModules.get(moduleSeal), "\nlater one: ", is);
		} else {
			importStatementsOfStarredModules.put(moduleSeal, is);
		}
	}

	private ImportStmt getImportStatementFor(Seal ms, Syntax src) throws FisherException {
		if (importStatementsOfStarredModules.containsKey(ms)) {
			return importStatementsOfStarredModules.get(ms);
		} else {
			Doom.internal("I thought we imported " + ms + ", but I don't know where it was imported.", src, ms);
			return null;
		}
	}

	public ModuleStatic findAvailableModule(Id moduleId, Syntax src) throws FisherException {
		String moduleName = moduleId.toString();
		// First of all, moduleId *might* be defined as a module in the
		// environment.
		if (this.hasSealFor(moduleId)) {
			Seal seal = this.seal(moduleId, src).dealias();
			if (seal.kind == SealKind.MODULE) {
				if (seal instanceof SealForModule) {
					SealForModule s4m = (SealForModule) seal;
					return s4m.moduleStatic;
				} else if (seal instanceof PwnedSeal) {
					PwnedSeal ps = (PwnedSeal) seal;
					return ps.moduleStatic;
				} else {
					Doom.internal("The kind says it's a module, but the type disagrees.", src, seal, seal.kind);
					return null;
				}
				
			} else {
				src.flag(DangerLevel.ERROR, "Not a module.", moduleId
						+ " is the name of something here, but not of a module.  There may be a module called "
						+ moduleId + " around as well, but the local name takes precedence, so I can't see the module");
				return null;
			}
		}
		// More likely, though, moduleId is the name of an available module
		else if (this.availableModules.containsKey(moduleName)) {
			return this.availableModules.get(moduleName);
		} else {
			src.flag(DangerLevel.ERROR, "No available module named " + moduleName, "");
			return null;
		}
	}

	public void addImportStar(Seal ms) {
		this.importStarredModules.add(ms);
	}


	public Env parent() {
		return this.parent;
	}
	
	public boolean anywhereInsideAClass(){return this.doingClass; }

	public boolean doingNew(){return this.doingNew; }
	
	public void setDoingNew(boolean b) {this.doingNew = b;}
	
	public ClassStatic classStatic() {
		return this.classStatic;
	}
	
	public void setClassStatic (ClassStatic cs) {
		this.classStatic = cs;
	}
	

	
}
