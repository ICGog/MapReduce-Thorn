
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.test;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import fisher.ingest.Ingester;
import fisher.parser.ParseException;
import fisher.parser.SyntacticClass;
import fisher.statics.ClassStatic;
import fisher.statics.Env;
import fisher.statics.FreeVariableSeals;
import fisher.statics.ModuleStatic;
import fisher.statics.Seal;
import fisher.statics.SealKind;
import fisher.statics.SealUtils;
import fisher.statics.Sealant;
import fisher.syn.ClsDecl;
import fisher.syn.FunDecl;
import fisher.syn.ImportStmt;
import fisher.syn.ItExp;
import fisher.syn.Module;
import fisher.syn.ModulesInAList;
import fisher.syn.Pat;
import fisher.syn.PatVar;
import fisher.syn.QueryTable;
import fisher.syn.Seq;
import fisher.syn.Table;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.syn.queries.PatVars_Defined_By_Pattern;
import fisher.util.Bard;
import fisher.util.Compilation;
import fisher.util.CompilerMessage;
import fisher.util.DangerLevel;
import fisher.util.FisherException;
import fisher.util.SpecialCharacters;
import junit.framework.TestCase;
import static fisher.statics.SealKind.*;
import static fisher.parser.SyntacticClass.*;

public  class  SealTest extends TestCase  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	public static final String SHOW="ßhøw";
	
	@Override
	protected void setUp() throws Exception {
		// TODO Auto-generated method stub
		super.setUp();
		SealUtils.clear();
		Compilation.current.reset();
	}

	public void test001_seal_basics() throws Exception {
		SealUtils.clear();
		assertEquals(0, SealUtils.ALL_SEALS.size());
		Seal s1 = new Seal(null, "phokion", MODULE, null, false);
		String sphokion = SpecialCharacters.SEAL + "phokion";
		assertEquals(sphokion, s1.toString());
		assertEquals(1, SealUtils.ALL_SEALS.size());
		// Make another that looks the same.
		Seal s2 = new Seal(null, "phokion", MODULE, null, false);
		assertEquals(sphokion, s1.toString());
		assertTrue(!(s1.equals(s2)));
		assertEquals(2, SealUtils.ALL_SEALS.size());
		// Make something in phokion
		Seal s3 = new Seal(null, "salamander", VAR, s1, false);
		String ssala = SpecialCharacters.SEAL + "phokion.salamander";
		assertEquals(ssala, s3.toString());
		assertEquals(s1.contents, Bard.list(s3));
		assertNull(s2.contents);
		assertEquals(s1, s3.container);
	}
	
	public static class ASym {
		public final SealKind kind;
		public final String str;
		public ASym(SealKind kind, String str) {
			super();
			this.kind = kind;
			this.str = str;
		}
		public String toString() {
			return "A·sym(" + str + ":" + kind + ")";
		}
		public boolean matches(Seal seal) {
			return seal.kind == kind && seal.str().equals(str);
		}
	}
	public static ASym asym(SealKind kind, String str) { return new ASym(kind, str);} 
	
	public void providesExactly(String loc, ModuleStatic modsta, ASym... asyms) throws Exception {
		int size = modsta.providedSeals().size();
		if (asyms.length != size) {
			assertEquals(loc + "/pE/count",asyms.length, size);
		}
		for (ASym asym : asyms) {
			int nMatches = 0;
			for (Seal seal : modsta.providedSeals()) {
				if (asym.matches(seal))  nMatches++;
			}
			String msg = null;
			if (nMatches == 0) {
				msg = "No seals match " + asym + ".\n";
					
			}
			else if (nMatches > 1){
				msg = "There were " + nMatches + " matches for "+ asym + "\n";
			}
			if (msg != null) {
				msg +=  "The symbols were: " + Bard.sep(modsta.providedSeals(), ", ") + "\n";
			} 
			assertEquals(loc + "/nMatch\n" + msg, 1, nMatches);
		}
	}
	
	public void test002_modulestatic() throws Exception {
		String loc = "002a";
		String moduleCode = "module SHAMPOO { var nutrient := 3; class logic(){} fun lurch()=1; val pleurisy = 2; }";
		ModulesInAList modulesInAList = (ModulesInAList) TestUtils.parse(loc, moduleCode, MODUS);
		List<Module> modules = modulesInAList.modules;
		List<ModuleStatic> statics = ModuleStatic.bareStaticsFor(modules);
		assertEquals(loc, statics.size(), modules.size());
		assertEquals(loc, statics.size(), 1);
		ModuleStatic modsta = statics.get(0);
		providesExactly(loc, modsta, asym(CLASS, "logic"), asym(FUN, "lurch"), asym(VAR, "nutrient"),
				asym(VAL, "pleurisy")
				);
	}
	
	private void checkPatVars(String loc, String code, String ... varnames) throws Exception {
		List<PatVar> patvars = patvars_defined_by_pattern(loc, code);
		assertEquals(loc + "/number", varnames.length, patvars.size());
		for(int i = 0; i < varnames.length; i++) {
			PatVar patVar = patvars.get(i);
			String varname = varnames[i];
			assertEquals(loc + "/name/[" + i + "]/" + varname, varname, patVar.id.str()); 
		}
		TestUtils.checkNoErrors(loc, "checkPatVars", code);
	}

	private List<PatVar> patvars_defined_by_pattern(String loc, String code)
			throws ParseException, FisherException {
		Compilation.current.reset();
		Pat pat = (Pat) TestUtils.parse(loc + "/parse", code, PAT);
		List<PatVar> patvars = PatVars_Defined_By_Pattern.of(pat);
		return patvars;
	}
	
	private void checkPatVar_ShouldFail(String loc, String code) throws Exception {
		List<PatVar> patvars = patvars_defined_by_pattern(loc, code);
		if (Compilation.current.messages.isEmpty()) {
			assertFalse(loc + "/checkPatVar_shouldFail", true);
		}
		else {
//			System.out.println("Woot!");
		}
	}
	
	public void test003_PatVars_defined_by_Pattern() throws Exception {
		checkPatVars("003a", "x", "x");
		checkPatVars("003b", "[x,y]", "x", "y");
		checkPatVars("003c", "x && y", "x", "y");
		checkPatVar_ShouldFail("003-b", "[x,x]");
		checkPatVar_ShouldFail("003-c", "x && y && x");
		checkPatVars("003d", "C(x,y)", "x", "y");
		checkPatVars("003e", "C([x,y], z)", "x", "y", "z");
		checkPatVar_ShouldFail("003-e", "C(x,y,D(z,x))");
		checkPatVars("003f", "C((it > 1)?)");
		checkPatVars("003g", "C(x, $(x), 1, 'yup')", "x");
		checkPatVars("003h", "!x");
		checkPatVars("003i", "it ~ [x,y]", "x", "y");
		checkPatVars("003j", "‹a:[x,y], b:!+_, c:C(c), d›", "x", "y", "c", "d");
		checkPatVar_ShouldFail("003n", "x || [x] || [[]]");
		checkPatVars("003k", "[x] || [1,x]", "x");
		checkPatVar_ShouldFail("003l", "x || !x");
		checkPatVar_ShouldFail("003m", "x || 1");
		checkPatVars("003o", "x || (1 && 1~x)", "x");
	}
	
	private static class Sealage {
		public String name;
		public SealKind kind;
		public String srcStr;
		public int nUsesDemanded;
		public List<Id> hit = new ArrayList<Id>();
		public Seal seal = null;
		public Sealage(String name, SealKind kind, String srcStr, int uses) {
			super();
			this.name = name;
			this.kind = kind;
			this.srcStr = srcStr;
			this.nUsesDemanded = uses;
		}
		public void addHit(String loc, Id id) throws Exception {
			Seal seal2 = id.seal();
			hit.add(id);
			if (this.seal == null) {
				this.seal = seal2;
			}
			else if (this.seal == seal2) {
				// all good
			}
			else {
				String msg = loc + "/addHit/" + name + "\n"
				+ "While hitting " + id + ", noticed that id already had the seal: \n"
				+ "   " + this.seal + "\n"
			    + "But I am trying to hit it with \n"
			    + "   " + seal2;
				assertTrue(msg, false);
			}
			Syntax def = seal2.def;
			
			// It is convenient sometimes to have a little leeway in what ancestor to compare string to.
			boolean codeMatches = sourceLooksLike(seal);
			if (!codeMatches) {
				String msg = 
					loc + "/defchk/" + this + " Definitions don't match. \n"
					+ "Source string = " + srcStr + "\n" 
					+ "And I tried: \n" + Bard.sep(reasonableCodes(seal), "\n")
					+ "\n(But it wasn't any of those.)"
					;
				assertTrue(msg, false);
			}
		}
		public boolean matches(Seal seal) {
			return name.equals(seal.str()) && kind == seal.kind
			 	&& (sourceLooksLike(seal))
			;
		}
		public boolean sourceLooksLike(Seal seal) {
			
			List<String> reasonableCodes = reasonableCodes(seal);
			if (srcStr.equals(SHOW)) {
				System.out.println(Bard.sep(reasonableCodes, "\n"));
			}
			return reasonableCodes.contains(srcStr);
		}
		private List<String> reasonableCodes(Seal seal) {
			Syntax def = seal.def;
			Syntax parent = def.parent();
			if (parent == null) return Bard.list(def.toString());
			List<String> reasonableCodes = 
				Bard.list(def.toString(), 
						parent.toString()
						);
			return reasonableCodes;
		}
		
		public String toString() {
			return name + "("+ kind + ")";
		}
	}
	
	public Sealage seel(String name, SealKind kind, String srcStr, int uses) {
		return new Sealage(name, kind, srcStr, uses);
	}
	

	
	public static boolean thereAreErrors() {
		for (CompilerMessage msg : Compilation.current.messages) {
			if (msg.danger == DangerLevel.ERROR) {
				return true;
			}
		}
		return false;
	}
	
	public static boolean thereAreWarnings() {
		for (CompilerMessage msg : Compilation.current.messages) {
			if (msg.danger == DangerLevel.WARNING) return true;
		}
		return false;
	}
	
	public static Syntax checkSeal(String loc, String code, SyntacticClass cls,
			String moduleCode,
			Sealage...sealages) throws Exception {
		Syntax syn = sealize(loc, code, cls, moduleCode);
		
		if (thereAreErrors()) {
			throw new Exception("\n" + loc + "\nThere were errors!\n " + Bard.sep(Compilation.current.messages, "\n"));
		}
		
		checkSealsOnAllIds(loc +"/seal/1", syn);
		checkSealsOnAllIds(loc +"/seal/1", modulesFromLastCallToSealize);
		
		List<Id> ids = Sealant.allIdsIn(syn);
		
		for (Id id : ids) {
			Seal seal = id.seal();
			for (Sealage sealage : sealages) {
				if (sealage.matches(seal)) {
					sealage.addHit(loc + "/id=" + id + "/sealage=" + sealage, id);
				}
			}
		}
		
		for (Sealage sealage : sealages) {
			// Count uses
			int usesDemanded = sealage.nUsesDemanded;
			int usesFound = sealage.hit.size();
			if (usesDemanded != usesFound) {
				String msg = loc + "/#/" + sealage.name + "\n"
				+ "Wrong number of uses of seal! For " + sealage.name + " of type "+ sealage.kind + " and code " + sealage.srcStr + "!\n"
				+ "WANTED = " + usesDemanded + "\n"
				+ "FOUND = " + usesFound + "\n" 
				+ "Their parents were : \n"+ 
				TestUtils.strParents(sealage.hit, "\n")
				+ "\n"
				;
				List<Seal> sealsForThatName = new ArrayList<Seal>();
				for (Id id : ids) {
					if (id.str().equals(sealage.name)) {
						sealsForThatName.add(id.seal());
					}
				}
				msg += "\nThe seals for that name that I found were: " + Bard.sep(sealsForThatName, ", ") + "\n";
				assertEquals(msg, usesDemanded, usesFound);
			}
		}
		return syn;

	}
	
	public static void checkSealsOnAllIds(String loc, Syntax syn){
		List<Id> ids = Sealant.allIdsIn(syn);
		for (Id id : ids) {
			Seal seal = id.seal();
			if (seal == null) {
				assertNotNull(loc + "/sealed/" + id + "\nIdentifier " + id + " is not sealed!\n line = "+ id.start.beginLine + " col = " + id.start.beginColumn + "\n" 
						+ id.original() + "\n"
						, seal);
			}
		}
	}
	
	public static List<ItExp> itExps(Syntax syn) {
		List<ItExp> L = new ArrayList<ItExp>();
		for (Syntax d : syn.computeDescendants()) {
			if (d instanceof ItExp) {
				ItExp it = (ItExp) d;
				L.add(it);
			}
		}
		return L;
	}
	
	
	public void checkSealFails(String loc, String code, SyntacticClass cls,
			String moduleCode,
			String... obligatoryStrings) throws Exception {
		Syntax syn = sealize(loc, code, cls, moduleCode);
		if (Compilation.current.messages.isEmpty()) {
			assertFalse(loc + "/fail/non-fail -- this should have had errors, but didn't.", true);
		}
		String glomMessages = Bard.sep(Compilation.current.messages, "\n\n");
		for (String string : obligatoryStrings) {
			if (!(glomMessages.contains(string))) {
				assertFalse(
					"The following obligatory error message is missing: \n" + string + "\n\nThe messages are: \n" + glomMessages + "\n",
					true
				);
			}
		}
	}

	
	public static ModulesInAList modulesFromLastCallToSealize; 
	public static Syntax synFromLastCallToSealize;
	public static Syntax sealize(String loc, String code, SyntacticClass cls,
			String moduleCode) throws ParseException, FisherException {
		Compilation.current.reset();
		// The modules...
		modulesFromLastCallToSealize = (ModulesInAList) TestUtils.parse(loc, moduleCode, MODUS);
		List<Module> modules = modulesFromLastCallToSealize.modules;
		Ingester.check(modulesFromLastCallToSealize);
		
		List<ModuleStatic> statics = ModuleStatic.bareStaticsFor(modules);
		
		// The code body
		Syntax syn = TestUtils.parse(loc + "/parse", code, cls);
		Ingester.check(syn);
		Sealant sealant = new Sealant();
		Env env = Env.root(statics);
		
		modulesFromLastCallToSealize.accept(sealant, env);
		
		syn.accept(sealant, env);
		synFromLastCallToSealize = syn;
		
		Sealant.finalCheck(syn);
		for (Module module : modules) {
			Sealant.finalCheck(module);
		}
		
		return syn;
	}
	
	public static final String MO = "module MO{var a; b = 1;} "; 
	public static final String NO = "module NO{} "; 
	public static final String YO = "module YO{var a; b = 2; var c;} ";
	public static final String RO = "module RO{var weezil;} ";
	
	public void test004() throws Exception {
		checkSeal("004a", "{a = 0; a;}", STMT, MO,
				seel("a", VAL, "a = 0", 2)
				);
		checkSeal("004b", "{a = 0; b = 0; a;}", STMT, MO, 
				seel("a", VAL, "a = 0", 2),
				seel("b", VAL, "b = 0", 1)
				);
		checkSeal("004c", "{import MO.a; a;}", STMT, MO,
				seel("a", VAR, "var a;", 2)
				);
		checkSeal("004d", "{import MO.a; a;}", STMT, MO + YO,
				seel("a", VAR, "var a;", 2)
				);
		checkSeal("004d", "{import YO.a; a;}", STMT, MO + YO,
				seel("a", VAR, "var a;", 2)
				);
		checkSeal("004e", "{import YO.a; import MO.b; a; b;}", STMT, MO + YO,
				seel("a", VAR, "var a;", 2),
				seel("b", VAL, "b = 1", 2)
				);
		checkSeal("004f", "{import YO.*; import MO.*; c;}", STMT, MO + YO,
				seel("c", VAR, "var c;", 1)
		);
		checkSeal("004f2", "{import YO.*; import MO.*; c; c;}", STMT, MO + YO,
				seel("c", VAR, "var c;", 2)
		);
		checkSealFails("004g", "{import YO.*; import MO.*; a;}", STMT, MO + YO,
				"More than one *-imported module defines a"
		);
		checkSeal("004f2", "{import YO.*; import MO.*; c; c;}", STMT, MO + RO + YO,
				seel("c", VAR, "var c;", 2)
		);
		checkSealFails("004g", "{import YO.*; import MO.*; a;}", STMT, MO + RO + YO,
				"More than one *-imported module defines a"
				);
		checkSeal("004h", "{ {a = 1; a;}  {b = 2; b;} }", STMT, MO,
				seel("a", VAL, "a = 1", 2),
				seel("b", VAL, "b = 2", 2)
				);
		checkSeal("004h", "{ {a = 1; a;}  {a = 2; a;} }", STMT, MO,
				seel("a", VAL, "a = 1", 2),
				seel("a", VAL, "a = 2", 2)
				);
		checkSeal("004i", "{ a = 1; { b = 2; a; b; } a;} ", STMT, MO,
				seel("a", VAL, "a = 1", 3),
				seel("b", VAL, "b = 2", 2)
		);
		checkSealFails("004i", "{ a = 1; { a = 2; a; a; } a;} ", STMT, MO,
				EvalTest.DUP_DEF
				);
		checkSealFails("004j", "{import MO;  MO = 1; }", STMT, MO, EvalTest.DUP_DEF);
		checkSeal("004k", "{a = 1; b = a; c = b;}", STMT, MO, 
				seel("a", VAL, "a = 1", 2),
				seel("b", VAL, "b = a", 2),
				seel("c", VAL, "c = b", 1)
			);
	}
	
	
	public void test005_sealing_assignments_and_operator_exps() throws Exception {
		checkSeal("005a", "{var a; a := 1;}", STMT, MO, seel("a", VAR, "var a;", 2));
		checkSealFails("005b", "{a := a; var a;}", STMT, MO, "a is not defined");
		checkSeal("005c", "{import MO.a; import MO.b; a := b;}", STMT, MO, 
				seel("a", VAR, "var a;", 2),
				seel("b", VAL, "b = 1", 2)
		);
		checkSeal("005d", "{import MO.a; import MO.b; a,a := b,b;}", STMT, MO, 
				seel("a", VAR, "var a;", 3),
				seel("b", VAL, "b = 1", 3)
		);
		checkSeal("005e", "{import MO.a; import MO.b; a,a := b,b; a := b; var c; c := a;}", STMT, MO, 
				seel("a", VAR, "var a;", 5),
				seel("b", VAL, "b = 1", 4),
				seel("c", VAR, "var c;", 2)
		);
		checkSeal("005f", "{var a := 1;}", STMT, MO, seel("a", VAR, "var a := 1;", 1));
		checkSeal("005g", "{b = 1; var a := b;}", STMT, MO, 
				seel("b", VAL, "b = 1", 2),
				seel("a", VAR, "var a := b;", 1)
				);
		checkSeal("005h", "1+2", EXP, MO);
		checkSeal("005i", "{a = 1+2*3; b = a < a < a;}", STMT, MO, 
				seel("a", VAL, "a = 1+2*3", 4),
				seel("b", VAL, "b = a < a < a", 1)
				);
	}
	
	public void test006_probe() throws Exception {
		checkSeal("006a", "~!@()", EXP, MO);
		checkSeal("006b", "~!@hit()", EXP, MO);
		checkSeal("006c", "{x = 1; ~!@hit(x);}", STMT, MO, 
				seel("x", VAL, "x = 1", 2)
		);
		checkSeal("006d", "{x = 1; ~!@hit(x);}", STMT, MO, 
				seel("x", VAL, "x = 1", 2)
		);
		checkSeal("006e", "~!@undef(x);", STMT, MO);
		// In the following: (a) x is defined, but that flaw isn't detected *here*.
		// (it will be detected dynamically we hope).
		// and (b), the appearance of 'x' as an arg of 'undef' doesn't count.
		checkSeal("006f", "{x = 1; ~!@undef(x);}", STMT, MO, seel("x", VAL, "x = 1", 1));
		checkSeal("006f", "{x = 1; ~!@undef(x, y);}", STMT, MO, seel("x", VAL, "x = 1", 1));
	}

//	public void test007_checking_recording_of_import_stars() throws Exception {
//		// I don't have a good way to generalize this, and I'm only doing it once, so here's some gronky code.
//		String loc = "007a";
//		String moduleCode = "module MU { var a; var b; var crash; } module NU { var c; var crash; }";
//		SyntacticClass cls = STMT;
//		String code = "{import MU.*; import NU.*; a := 1; a := 2; b := 3; c := 4;}";
//		Syntax syn = sealize(loc, code, cls, moduleCode);
//		ImportStmt iMU = (ImportStmt) ((Seq)syn).cmds.get(0);
//		ImportStmt iNU = (ImportStmt) ((Seq)syn).cmds.get(1);
//		assertEquals(loc + "/iMU/size", iMU.membersToImport.size(), 2);
//		Seal smu0 = iMU.membersToImport.get(0);
//		Seal smu1 = iMU.membersToImport.get(1);
//		assertEquals(loc + "/smu0/a", "a", smu0.str());
//		assertEquals(loc + "/smu1/b", "b", smu1.str());
//		assertEquals(loc + "/iNU/size", iNU.membersToImport.size(), 1);
//		Seal snu0 = iNU.membersToImport.get(0);
//		assertEquals(loc + "/snu0/c", "c", snu0.str());
//	}
	
	public void test008_import_star() throws Exception {
		
		String VO = "module VO { var a; var b; var c; var d; }";
		checkSeal("008a", "{import VO.*;  a := 1; {import VO.*; a := 2; b := 3; }}", STMT, VO,
				seel("a", VAR, "var a;", 2),
				seel("b", VAR, "var b;", 1)
		);
		checkSeal("008b", "{import VO.*;  a := 1; {import VO.*; a := 2; b := 3; } a := 4; c := 5;}", STMT, VO,
				seel("a", VAR, "var a;", 3),
				seel("b", VAR, "var b;", 1),
				seel("c", VAR, "var c;", 1)
		);
		checkSeal("008c", "{import VO.*;import VO.*;  a := 1; {import VO.*; a := 2; b := 3; } a := 4; c := 5;}", STMT, VO,
				seel("a", VAR, "var a;", 3),
				seel("b", VAR, "var b;", 1),
				seel("c", VAR, "var c;", 1)
				);
	}
	
	public void test009_class() throws Exception {
//		checkSeal("009a", "{class c{}}", STMT, NO,
//				seel("c", CLASS, "class c{}", 1)
//				);
//		{
//			String b = "class c{new c() = {}}";
//			checkSeal("009b", "{" + b + "}", STMT, NO,
//					seel("c", CLASS, b, 3) // It looks like 2, but there's one in the MonoBody and one in the ctor itself.
//			);
//		}
//		{
//			String b = "class c{new c() = {} pat p(x) = {x = 1;}}";
//			checkSeal("009b", "{" + b + "}", STMT, NO,
//					seel("c", CLASS, b, 3), // It looks like 2, but there's one in the MonoBody and one in the ctor itself.
//					seel("p", PAT_SEAL, "pat p(x) = {x = 1;}", 1),
//					seel("x", PAT_FORMAL, "pat p(x) = {x = 1;}", 2)
//			);
//		}
//		{
//			String m = "def m(a,b) = a+b;";
//			String b = "class c{new c() = {} pat p(x) = {x = 1;} " + m + "}";
//			checkSeal("009c", "{" + b + "}", STMT, NO,
//					seel("c", CLASS, b, 3), // It looks like 2, but there's one in the MonoBody and one in the ctor itself.
//					seel("p", PAT_SEAL, "pat p(x) = {x = 1;}", 1),
//					seel("x", PAT_FORMAL, "pat p(x) = {x = 1;}", 2),
//					seel("m", METHOD, m, 2),
//					seel("a", VAL, "(a,b)", 2),
//					seel("b", VAL, "(a,b)", 2)
//			);
//		}
//		{
//			String m = "def m(a,b) = a+b;";
//			String v = "v = 1";
//			String b = "class c{new c() = {} pat p(x) = {x = 1;} " + m + " " + v + ";}";
//			checkSeal("009d", "{" + b + "}", STMT, NO,
//					seel("c", CLASS, b, 3), // It looks like 2, but there's one in the MonoBody and one in the ctor itself.
//					seel("p", PAT_SEAL, "pat p(x) = {x = 1;}", 1),
//					seel("x", PAT_FORMAL, "pat p(x) = {x = 1;}", 2),
//					seel("m", METHOD, m, 2),
//					seel("a", VAL, "(a,b)", 2),
//					seel("b", VAL, "(a,b)", 2),
//					seel("v", VAL, v, 1)
//			);
//		}
//		{
//			String m = "def m(a,b) = a+b;";
//			String v = "v = 1";
//			String y = "var y := 2+v;";
//			String b = "class c{new c() = {} pat p(x) = {x = 1;} " + m + " " + v + "; " +  y + "}";
//			checkSeal("009e", "{" + b + "}", STMT, NO,
//					seel("c", CLASS, b, 3), // It looks like 2, but there's one in the MonoBody and one in the ctor itself.
//					seel("p", PAT_SEAL, "pat p(x) = {x = 1;}", 1),
//					seel("x", PAT_FORMAL, "pat p(x) = {x = 1;}", 2),
//					seel("m", METHOD, m, 2),
//					seel("a", VAL, "(a,b)", 2),
//					seel("b", VAL, "(a,b)", 2),
//					seel("v", VAL, v, 2),
//					seel("y", VAR, y, 1)
//			);
//		}
//		{
//			String m = "def m(a,b) = a+b;";
//			String v = "v = 1";
//			String y = "var y := 2+v+phy;";
//			String PHO = "module PHO{var pha; phy = 1;} ";
//			String b = "class c{import PHO.*; new c() = {} pat p(x) = {x = 1;} " + m + " " + v + "; " +  y + "}";
//			checkSeal("009f", "{" + b + "}", STMT, PHO,
//					seel("c", CLASS, b, 3), // It looks like 2, but there's one in the MonoBody and one in the ctor itself.
//					seel("p", PAT_SEAL, "pat p(x) = {x = 1;}", 1),
//					seel("x", PAT_FORMAL, "pat p(x) = {x = 1;}", 2),
//					seel("m", METHOD, m, 2),
//					seel("a", VAL, "(a,b)", 2),
//					seel("b", VAL, "(a,b)", 2),
//					seel("v", VAL, v, 2),
//					seel("y", VAR, y, 1),
//					seel("phy", VAL, "phy = 1", 1)
//			);
//		}		
	}
	
//	public void test010_class_2() throws Exception {
//		String sip = "meth sip() = 1;";
//		String daunt = "meth daunt() = 2;";
//		String abandoned = "pat abandoned(x) = {x = 1;}";
//		String weary = "pat weary(y) = {y = 1;}";
//		String example = "example = 3";
//		String overtone = "overtone = 4";
//		String incident = "var incident := 5;";
//		String wealth = "var wealth := 6;";
//		String clsDecl = 
//			"class Hussy{" +//
//			sip + " " +//
//			daunt + " " +//
//			abandoned + " " +//
//			weary + " " +//
//			example + "; " +//
//			overtone + "; " +//
//			incident + " " +//
//			wealth + " " +//
//			"}";
//		checkSeal("010", clsDecl, CLS, MO,
//				seel("sip", METHOD, sip, 2) // Note double-counting from method/monobody
//				,seel("daunt", METHOD, daunt, 2) //
//				,seel("abandoned", PAT_SEAL, abandoned, 1)// pats don't double-count.
//				,seel("weary", PAT_SEAL, weary, 1)// 
//				,seel("example",VAL, example, 1)//
//				,seel("overtone",VAL, overtone, 1)//
//				,seel("incident",VAR, incident, 1)//
//				,seel("wealth",VAR, wealth, 1)//
//		);
//		ClsDecl cd = (ClsDecl) synFromLastCallToSealize;
//		
//		ClassStatic cs = cd.classStatic;
//		assertEquals("010a", 2, cs.methods.size());
//		assertEquals("010b", 2, cs.pats.size());
//		assertEquals("010b", 4, cs.instanceInitCode.size());
//		assertEquals("010c",
//				Bard.list("sip", "daunt", "abandoned", "weary", "example", "overtone", "incident", "wealth"),
//				cs.definedNames
//		);
//		assertEquals("010d", true, cs.hasDefaultConstructor);
//		assertEquals("010e", cs.methods.keySet(), Bard.set("sip", "daunt"));
//		assertEquals("010e", cs.pats.keySet(), Bard.set("weary", "abandoned"));
//		assertEquals("010f", cs.classSeal, cd.name.seal());
//	}
	
	
	public void test011_while() throws Exception {
		checkSeal("011a", "while(1){2;}", STMT, MO);
		{
			String code = "a: while(1){2; break a;}";
			checkSeal("011b", code, STMT, MO,
					seel("a", LOOP_NAME, code, 2)
			);	
		}
		{
			String inner  = "b: while(3){break a; continue b;}";
			String code = "a: while(1){2; " + inner + "}";
			checkSeal("011b", code, STMT, MO,
					seel("a", LOOP_NAME, code, 2),
					seel("b", LOOP_NAME, inner, 2)
					);	
		}
		
		checkSealFails("011c", "break b;", STMT, MO, "b is not defined");
		checkSealFails("011c", "{b: while(1){1;} break b;}", STMT, MO, "b is not defined");
	}
	
	public void test012_duplicate_binding_in_module() throws Exception {
		String MYMO = "module M {var a; var a;} ";
		checkSealFails("012a", "{}", STMT, MYMO, EvalTest.DUP_DEF);
		checkSealFails("012a", "{}", STMT, "module M {var a; fun a()=1;} ", EvalTest.DUP_DEF);
	}
	
	private static class FV{
		String varname;
		int count;
		public FV(String varname, int count) {
			super();
			this.varname = varname;
			this.count = count;
		}
	}
	private static FV fv(String s) {return new FV(s,-1);}
	
	private void checkFreeVars(String loc, String code, Class focus, int which, FV... fvs) throws Exception{
		Syntax syn = sealize(loc, code, STMT, "module M{}");
		if (! Compilation.current.messages.isEmpty()) {
			assertFalse(loc + "/fv/compilation -- this shouldn't have produced compiler errors:\n"
					+ Bard.sep(Compilation.current.messages, "\n")
					, true);
		}
		List<Syntax> descs = syn.computeDescendants();
		Syntax theone = null;
		int k = 0;
		for (Syntax syntax : descs) {
			if (syntax.getClass() == focus) {
				k ++;
				if (k == which)
					theone = syntax;
				}
			}
		assertNotNull(loc + "/theOne/notnull", theone);
		// theone == the one and only descendant of class focus.
		Set<Seal> fvSeals = FreeVariableSeals.of(theone);
		if (fvs.length != fvSeals.size()) {
			assertEquals(loc + "/#", fvs.length, fvSeals.size());
		}
		for (FV fv : fvs) {
			// Check that there is exactly one seal in fvSeals with name == a.
			int sealsNamedThat = 0;
			for (Seal seal : fvSeals) {
				if (seal.str().equals(fv.varname)) sealsNamedThat += 1;
			}
			assertEquals(loc + "/" + fv.varname + "/count", 1, sealsNamedThat);
		}
	}
	
	
	public void test013_fv() throws Exception {
		FV a = fv("a");
		checkFreeVars("012a", "{var a := 1; fun b()=a;}", FunDecl.class, 1, a);
		FV b = fv("b");
		checkFreeVars("012b", "{a=1;b=2; fun c()=[a,b, (fn d = {:e:1:})];}", FunDecl.class, 1,  a, b);
		checkFreeVars("012c", "{a=1; class B{b = a;} }", ClsDecl.class, 1, a);
		checkFreeVars("012c", "{a=1; class B{b = a; def m()=[a,b];} }", ClsDecl.class, 1, a);
		FV x = fv("x");
		checkFreeVars("012c", "{a=1; x=1; class B{b = a; def m()=[a,b,x];} }", ClsDecl.class, 1, a, x);
		checkFreeVars("012c", "{a=1; x=1; table(k){var w,bloob;}; }", Table.class, 1);
		checkFreeVars("012c", "{a=1; x=1; %table(k=j){var w:=a,bloob:=x; | for j <- a .. x}; }", QueryTable.class, 1, a, x);
	}
	
	public void test014_spawn() throws Exception {
		checkSeal("014a", "spawn{body{}}", EXP, NO);
		checkSeal("014b", "spawn{a=1; body{a;}}", EXP, NO);
		checkSeal("014c", "spawn{a=1; initially{a;} body{a;}}", EXP, NO);
		checkSeal("014d", "spawn{a=1; sync p(x){a;x;} initially{a;} body{a;}}", EXP, NO);
		checkSeal("014e", "spawn{a=1; async p(x){a;x;} initially{a;} body{a;}}", EXP, NO);
		// Not Yet: checkSeal("014e", "spawn{a=1; fun f() = 1; async p(x){a;x;} init{a;} body{a;}}", EXP, NO);
	}
	
	public void test015_vardecl() throws Exception {
		checkSeal("015a", "{var x;}", STMT, NO);
		checkSeal("015b", "{var x:int;}", STMT, NO);
		checkSeal("015c", "{var x:int := 3;}", STMT, NO);
		checkSeal("015d", "{class C{} var x:C; }", STMT, NO);
		checkSeal("015d", "{class C{} var x:C := C(); }", STMT, NO);
		checkSeal("015d", "{class C{} var x:C & bool := C(); }", STMT, NO);
		
	}
	
}
