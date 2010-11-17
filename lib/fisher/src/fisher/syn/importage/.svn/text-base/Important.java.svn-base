
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.syn.importage;

import fisher.statics.Env;
import fisher.statics.ModuleStatic;
import fisher.statics.PwnedSeal;
import fisher.statics.Seal;
import fisher.statics.SealForModule;
import fisher.statics.SealKind;
import fisher.syn.ImportStmt;
import fisher.syn.QualName;
import fisher.syn.core.Id;
import fisher.util.DangerLevel;
import fisher.util.FisherException;

/**
 * @author bard A reagant is something which reacts. A contaminant is something
 *         which contaminates. An important is something which imports, ne?
 */
public enum Important {
	IMPORT_A_MODULE() {
		public String explanation() {
			return "'import MOOD = MOD;' and 'import MOD;'";
		}

		@Override
		public Env trySealing(ImportStmt is, Env env) throws FisherException {
			// Check that 'is' is one of these...
			if (is.isOwn == true)
				return null;
			QualName modName = is.modName;
			if (modName.ids.size() != 1)
				return null;
			if (is.modArgs.size() != 0)
				return null;
			// 'as' is allowed.
			if (is.dotstar == true)
				return null;
			Id MOD = modName.ids.get(0);
			Id MOOD = is.as == null ? MOD : is.as;
			// MOD had better be a module in the import list...
			ModuleStatic ms = env.findAvailableModule(MOD, is);
			if (ms == null) {
				is.flag(DangerLevel.ERROR, "I know of no module named " + MOD, "", MOD);
				return env;
			}
			assert (ms != null);
			Seal moduleSeal = ms.moduleSeal();

			// MOD gets sealed with moduleSeal. We won't actually use the name
			// MOD later
			// but every Id needs a seal.
			// (unless MOOD actually is the same name as MOD, which actually is
			// the standard case)
			if (MOD != MOOD) {
				MOD.setSeal(moduleSeal, is);
			}

			// make MOOD be an alias for MOD.
			Seal moodSeal = moduleSeal.alias(MOOD.str());
			moodSeal.setImportStmt(is);

			// MOOD might be in the environment already (e.g., if we've got
			// "import MOD; .... import MOD;")
			// So check for that.
			Seal extantMoodSeal = null;
			if (env.hasSealFor(MOOD.str())) {
				extantMoodSeal = env.seal(MOOD.str(), is);
				if (!extantMoodSeal.isAnAliasFor(moduleSeal)) {
					// BAD: MOOD is bound to something else.
					Env.duplicateDefinitionDoom(MOOD.str(), moodSeal, env);
				} else {
					// GOOD: the old definition is for the same thing. Let's
					// just use the old one.
					moodSeal = extantMoodSeal;
				}
			}

			// OK, now we can finally write it down.
			MOOD.setSeal(moodSeal, is);

			// MOOD *will* be used later, so it gets into the env.
			env.define(MOOD.str(), moodSeal);

			// Write down this stuff in the import statement itself for later
			// reference.
			AbstractImport ai = new ImportAModule(is, MOD, MOOD, ms, moodSeal);
			is.importage = ai;

			return env;

		}
	}// IMPORT_A_MODULE
	,
	IMPORT_A_MEMBER {
		@Override
		public String explanation() {
			return "'import doom = MOD.mood;' and 'import MOD.mood;'";
		}

		@Override
		public Env trySealing(ImportStmt is, Env env) throws FisherException {
			// Check that 'is' is one of these...
			if (is.isOwn == true)
				return null;
			QualName modName = is.modName;
			if (modName.ids.size() != 2)
				return null; // This is the only difference from
			// IMPORT_A_MODULE
			if (is.modArgs.size() != 0)
				return null;
			// 'as' is allowed.
			if (is.dotstar == true)
				return null;
			Id MOD = modName.ids.get(0);
			Id mood = modName.ids.get(1);
			Id doom = is.as == null ? mood : is.as;
			// MOD had better be a module...

			// MOD might be an alias of some other module, in which case it
			// needs to be loaded already:
			// code would look like: import MOD = FoD; import doom = MOD.mood;
			boolean moduleShouldBeLoadedAlready = env.hasSealFor(MOD);

			ModuleStatic ms = env.findAvailableModule(MOD, is);
			if (ms == null) {
				is.flag(DangerLevel.ERROR, "I know of no module named " + MOD, "", MOD);
				return env;
			}
			assert (ms != null);
			//			Seal moduleSealWRONG = ms.moduleSeal();
			Seal moduleSeal = moduleShouldBeLoadedAlready ? env.seal(MOD, is) : ms.moduleSeal();

			// MOD must get a seal.
			MOD.setSeal(moduleSeal, is);

			// mood must get a seal;
			Seal moodSeal = ms.findSealForMember(mood.str(), is);
			if (moodSeal == null) {
				// No need to re-flag the error, since findSealForMember does
				// that.
				return env;
			}
			if (mood != doom) {
				mood.setSeal(moodSeal, is);
			} else {
				// They're the same Id, so we'll use the more uniform alias
				// symbol.
			}

			// the seal of doom!
			Seal doomSeal = moodSeal.alias(doom.str());
			doomSeal.setImportStmt(is);
			
			doom.setSeal(doomSeal, is);
			env.define(doom.str(), doomSeal);

			AbstractImport ai = new ImportAField(is, MOD, mood, doom, ms, moduleSeal, moduleShouldBeLoadedAlready,
					moodSeal, doomSeal);
			is.importage = ai;

			return env;

		}
	},
	IMPORT_STAR {
		@Override
		public String explanation() {
			return "'import MOD.*;'";
		}

		@Override
		public Env trySealing(ImportStmt is, Env env) throws FisherException {
			// Check that 'is' is one of these...
			if (is.isOwn == true)
				return null;
			QualName modName = is.modName;
			if (modName.ids.size() != 1)
				return null;
			if (is.modArgs.size() != 0)
				return null;
			// no 'as' here.
			if (is.as != null)
				return null;
			if (is.dotstar == false)
				return null;
			Id MOD = modName.ids.get(0);

			if (env.hasSealFor(modName)) {
				Seal modSeal0 = env.seal(modName, is);
				
				Seal daSeal = modSeal0.dealias();
				assert (modSeal0 != null);
				if (modSeal0.kind != SealKind.MODULE) {
					is.flag(DangerLevel.ERROR, "Not a module", modName + " is not a module; it is a " + modSeal0.kind);
					return env;
				}
				SealForModule modSeal = (SealForModule) (modSeal0.dealias());
				env.addImportStar(modSeal);
				env.noteImportStar(modSeal, is);
				MOD.setSeal(modSeal, is);
				ModuleStatic ms = modSeal.moduleStatic;
				ImportModuleStar impModStar = new ImportModuleStar(is, ms, modSeal);
				is.importage = impModStar;
				return env;
			} else {

				ModuleStatic ms = env.findAvailableModule(MOD, is);
				if (ms == null) {
					is.flag(DangerLevel.ERROR, "I know of no module named " + MOD, "", MOD);
					return env;
				}
				assert (ms != null);
				SealForModule moduleSeal = ms.moduleSeal();
				MOD.setSeal(moduleSeal, is);
				env.addImportStar(moduleSeal);
				env.noteImportStar(moduleSeal, is);

				ImportModuleStar impModStar = new ImportModuleStar(is, ms, moduleSeal);
				is.importage = impModStar;

				return env;
			}

		}

	} // IMPORT_STAR
	,
	IMPORT_OWN {
		@Override
		public String explanation() {
			return "'import own MOOD = MOD;'";
		}

		@Override
		public Env trySealing(ImportStmt is, Env env) throws FisherException {
			// Check that 'is' is one of these...
			if (is.isOwn == false)
				return null;
			QualName modName = is.modName;
			if (modName.ids.size() != 1)
				return null;
			if (is.modArgs.size() != 0)
				return null;
			// 'as' is required
			if (is.as == null)
				return null;
			if (is.dotstar == true)
				return null;

			Id MOD = modName.ids.get(0);
			Id MOOD = is.as;
			assert (MOOD != null);

			ModuleStatic ms = env.findAvailableModule(MOD, is);
			if (ms == null) {
				is.flag(DangerLevel.ERROR, "I know of no module named " + MOD, "", MOD);
				return env;
			}
			assert (ms != null);
			SealForModule moduleSeal = ms.moduleSeal();

			MOD.setSeal(moduleSeal, is); // This is the seal of MOD, which is not used beyond this.

			PwnedSeal pwnedSeal = moduleSeal.own(MOOD.str(), is);
			pwnedSeal.setImportStmt(is);
			
			MOOD.setSeal(pwnedSeal, is);
			env.define(MOOD.str(), pwnedSeal);
			ImportOwnModule iom = new ImportOwnModule(is, ms, pwnedSeal);
			is.importage = iom;

			return env;
		}
	};
	public abstract String explanation();

	/**
	 * @param is
	 * @return an AbstractImport of this kind which describes this import stmt,
	 *         or null if none do.
	 */
	public abstract Env trySealing(ImportStmt is, Env env) throws FisherException;

	public static String approvedForms() {
		StringBuffer sb = new StringBuffer();
		for (Important important : Important.values()) {
			sb.append("  " + important.explanation() + "\n");
		}
		return sb.toString();
	}
}
