
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

import java.io.File;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import fisher.eval.Evaller;
import fisher.parser.ParseException;
import fisher.parser.SyntacticClass;
import fisher.run.FreakSyntax;
import fisher.runtime.IntTh;
import fisher.runtime.ListTh;
import fisher.runtime.Thing;
import fisher.syn.Cmd;
import fisher.syn.Probe;
import fisher.syn.core.Syntax;
import fisher.util.Bard;
import fisher.util.FisherException;
import junit.framework.TestCase;
import static fisher.statics.SealKind.*;
import static fisher.test.EvalTest.evalFiles;
import static fisher.test.TestUtils.printCount;
import static fisher.parser.SyntacticClass.*;


public  class  EvalTest extends TestCase  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public static final String WRONG_NUM = "not hit the right number of times";

	public static Syntax prep(String loc, String code, SyntacticClass cls,
			String moduleCode) throws Exception {
		return SealTest.checkSeal(loc, code, cls, moduleCode);
	}

	
	public static final String MO = "module MO{} ";
	public static final String LO = "module LO{la = 'la'; lay = 'lay'; lie = 'lie'; lye = 'lye'; var crash;} ";
	public static final String VO = "module VO{var vae; var vow; var view; var vo; var vent; var crash;} ";
	public static final String PHO = "module PHO{var c := 0; inc = fn(){c := c + 1; c;};} ";
	public static final String HOFO = "module HOFO{twice = fn(f)=(fn(x)=f(f(x))); inc = fn(x)=x+1;}";
	public static final String CLO = "module CLO{ class Canoe{}  class Loser{def vex(){}}Loser  var feud:=1; sector=2; }";
	
	public void test000_freaky_converter() {
		if (FreakSyntax.convert) {
			FreakSyntax.main(null);
		}
		System.out.println("Done!");
	}
	
	public void test001_Th_Int() throws Exception {
		IntTh z = IntTh.of(0);
		IntTh one = IntTh.of(1);
		assertEquals(z,z);
		assertEquals(one, one);
		assertFalse(z.equals(one));
		assertFalse(one.equals(z));
		assertEquals(z.toString(), "0");
		assertEquals(one.toString(), "1");
	}
	
	public void evalsTo(String loc, String code, SyntacticClass cls, String moduleCode, Thing thing) throws Exception {
		TestUtils.reset(loc);
		Cmd cmd = (Cmd) prep(loc + "/prep", code, cls, moduleCode);
		Thing res = Evaller.fullEval(cmd);
		assertEquals(loc + "/equals", thing, res);
		checkProbes(loc, cmd);
	}

	public static void  evals(String loc, String code, SyntacticClass cls, String moduleCode) throws Exception {
		TestUtils.reset(loc);
		Cmd cmd = (Cmd) prep(loc + "/prep", code, cls, moduleCode);
		try {
			Thing res = Evaller.fullEval(cmd);
		}
		catch (FisherException fe) {
			throw new Exception(loc + "/eval", fe);
		}
		catch (RuntimeException re) {
			throw new Exception(loc + "/eval", re);
		}
		checkProbes(loc, cmd);
	}
	
	public static void checkProbes(String loc, Cmd cmd) {
		List<Probe> probes = TestUtils.probesIn(cmd);
		boolean seenCheckPhase = false;
		for (Probe probe : probes) {
			int hits = TestUtils.hitCount(probe);
			int wanted = probe.count;
			if (wanted == -1 || wanted == hits) {
				// All is well here
			}
			else {
				String msg =
					loc + "/#hit/\n" +
					"A probe was not hit the right number of times.\n"
					+ probe + "\n" 
					+ "Wanted number = " + wanted + "\n"
					+ "Actually hit  = " + hits + "\n"
					+ probe.original();
					;
				Map<Probe, Integer> hitCounts = new HashMap<Probe, Integer>(TestUtils.hitCounts);
				assertTrue(msg, false);
			}
			if (probe.prober.equals(Prober.CHECKPHASE)) seenCheckPhase = true;
		}
		
	}
	
	public static boolean hasCheckPhase(Cmd cmd) {
		for (Probe probe : TestUtils.probesIn(cmd)) {
			if (probe.prober.equals(Prober.CHECKPHASE)) return true;
		}
		return false;
	}
	
	public static void evalFails(String loc, String code, SyntacticClass cls, String moduleCode, String... required) throws Exception {
		evalFailsArr(loc, code, cls, moduleCode, required);
	}
	public static void evalFailsArr(String loc, String code, SyntacticClass cls, String moduleCode, String[] required) throws Exception {
		boolean result = true;
		try {
			TestUtils.reset(loc);
			Cmd cmd = (Cmd) prep(loc + "/prep", code, cls, moduleCode);
			Thing res = Evaller.fullEval(cmd);
			checkProbes(loc, cmd);
			result = false;
		}
		catch(Throwable ex) {
			checkRequiredStrings(loc, ex, required);
		}
		if (!result) {
			assertFalse(loc + "/should-have-failed", true);
		}
	}

	public static void evalFiles(String loc, String filename, String moduleCode) throws Exception {
		evalFiles(loc, filename, moduleCode, 0);
	}
	
	public static void evalFiles(String loc, String filename, String moduleCode, int depth) throws Exception {
		TestUtils.say(TestUtils.test, "starting evalFiles(" + loc + ", " + filename + ",_," + depth);
		TestUtils.testScribble = "Loc: " + loc + "; Filename = " + filename;
		try {
			File file = TestUtils.testfile("eval/" + filename);
//			Sys	tem.out.println("fmp-file=" + file);
			if (file.isDirectory()) {
				// Skip ...tmp (which will be used for file test)
				if (file.getName().endsWith("tmp")) return;
				
				
				String[] fns = file.list();
				for (String string : fns) {
					String subfile = filename + "/" + string;
					if (string.endsWith(".th")) {
//						System.out.println("fmp-.th" + string);
						evalFiles(loc + "/" + string, subfile, moduleCode, depth+1);
					}
					else if (string.startsWith(".")) {
						// ignore
					}
					else if (string.endsWith("~")) {
						// ignore
					}
					else if (string.endsWith("thm")) {
						// ignore
					}
					else if (string.endsWith("el")) {
						// ignore
					}
					else if (string.contains(".#")) {
						// ignore
					}
					else if (string.endsWith(".txt") || string.endsWith(".html")) {
						// ignore
					}
					else {
						evalFiles(loc + "/" + string, subfile, moduleCode, depth+1);
					}
				}
			}
			else {
				if (! file.canRead() ) {
				
					if (depth <= 2) {
						assertTrue("I don't think the directory exists: " + file, false);
					}
					return; 
				}
				String contents = Bard.contentsOf(file);
//				System.out.println("zoixoz, from (" + file + ") we get ...\n" + contents);
				if (contents == null) {
					throw new Exception(loc + "/file=" + file + " -- test file is empty!");
				}
				if (TestUtils.PRINT_FILES) System.err.println("Doing file=" + file);
				String locfi = loc + "\n file = " + file + "\n";
				if (contents.startsWith("//FAIL")) {
					evalFails(locfi, contents, STMT, moduleCode, failies(contents));
				}
				else {
					evals(locfi, contents, STMT, moduleCode);
				}
			}
		}
		finally {
			TestUtils.say(TestUtils.test, "ending evalFiles(" + loc + ", " + filename + ",_," + depth);
			TestUtils.testScribble = null;
		}
	}// evalFiles
	
	public static String[] failies(String s) {
		List<String> L = new ArrayList<String>();
		int i = 0;
		do {
			String faily = "//FAIL:";
			int j = s.indexOf(faily, i);
			if (j < 0) break;
			int k = s.indexOf("\n", j);
			if (k < 0) k = s.length()-1;
			i = k;
			L.add(s.substring(j + faily.length(),k));
		}
		while(true);
		String[] A = new String[L.size()];
		L.toArray(A);
		return A;
	}
	
	private static void checkRequiredStrings(String loc, Throwable ex,
			String... required) {
		String s = ex.toString();
		for (String string : required) {
			if (! s.contains(string)) {
				assertFalse(loc + "/failure/" + "\nFailure message should have contained the string '" + string + "'"
						+ "\nmessage is \n" + s
						,
						true);
				
			}
		}
	}
	
	public void test002_minimal_eval() throws Exception {
		evalsTo("002a", "1", EXP, MO, IntTh.of(1));
		evalsTo("002b", "{1;2;}", STMT, MO, IntTh.of(2));
		evalsTo("002c", "{var a := 2; 1;2;a;}", STMT, MO, IntTh.of(2));
		evalsTo("002d", "{var a := 2; 1;{2;a;}}", STMT, MO, IntTh.of(2));
		evalsTo("002e", "{var a := 2; 1;{var b := a;2;b;}}", STMT, MO, IntTh.of(2));
		evalsTo("002f", "{~!@hit() @!~1; 1;}", STMT, MO, IntTh.of(1));
		evalFails("002f", "{~!@hit()@!~2;}", STMT, MO, WRONG_NUM);
		evalsTo("002g", "{a = 1; a;}", STMT, MO, IntTh.of(1));
		evalsTo("002h", "{a = 1; var b; b := a; b;}", STMT, MO, IntTh.of(1));
		evalsTo("002i", "{~!@eq(1,1,1,1) @!~ 1; 123;}", STMT, MO, IntTh.of(123));
		evalFails("002i", "{~!@eq(1,1,1,1) @!~ 2; 123;}", STMT, MO, WRONG_NUM);
		evalFails("002i", "{~!@eq(1,1,1,1) @!~ 0; 123;}", STMT, MO, WRONG_NUM);
		evalFails("002j", "{~!@eq(1,1,2,1) @!~ 0; 123;}", STMT, MO, "Failure of ~!@eq");
	}
	
	public void test003_evals_assortment() throws Exception {
		evals("003a", "{a = 1; b = 1; var c := 1; var d; d := 1; var e := a; var f; f := b; ~!@eq(a,b,c,d,e,f)@!~1; }", STMT, MO);
		evalFails("003a", "{a = 1; b = 1; var c := 1; var d; d := 1; var e := a; var f; f := b; doom = 2; ~!@eq(a,b,c,d,e,f, doom)@!~1; }", STMT, MO, "Failure of ~!@eq");


		evalFails("003d", "{ y = 2;~!@undef(x,y)@!~;}", STMT, MO);
		evalFails("003d", "{x = 1; y = 2;~!@undef(x,y)@!~;}", STMT, MO);
		evalFails("003d", "{x = 1; ~!@undef(x,y)@!~;}", STMT, MO);
		evals("003d", "{juft = 1; ~!@undef(x,y)@!~;}", STMT, MO);
		
		evalFails("003e", "{~!@isdef(x,y)@!~;}", STMT, MO);
		evalFails("003e", "{x=0;~!@isdef(x,y)@!~;}", STMT, MO);
		evalFails("003e", "{y=3;~!@isdef(x,y)@!~;}", STMT, MO);
		evals("003e", "{x=3;y=5;~!@isdef(x,y)@!~;}", STMT, MO);
		
		evals("003c", "{~!@undef(nope)@!~;}", STMT, MO);
		evals("003c", "{nope = 1; ~!@isdef(nope)@!~;}", STMT, MO);
		evalFails("003h", "{a=1;a=2;}", STMT, MO, DUP_DEF);
		evalFails("003h", "{a=b;}", STMT, MO, "not defined");
		evals("003i", "{x = 2; ~!@eq(x-1, 1)@!~;}", STMT, MO);
	}
	
	public void test004_math() throws Exception {
		evals("004a", "{~!@eq(3, 1+2, 3+0, 1+1+1) @!~1;}", STMT, MO);
		evals("004b", "{var a := 1; b = 2; ~!@eq(a+b, a+2, 3, b+1, b+a)@!~1;}", STMT, MO);
		evals("004c", "true;", STMT, MO);
		evals("004d", "~!@eq(true, true) @!~1;", STMT, MO);
		evals("004d", "~!@eq(true, true && true) @!~1;", STMT, MO);
		evals("004d", "~!@eq(true, true && true && true) @!~1;", STMT, MO);
		evals("004e", "~!@eq(false, false) @!~1;", STMT, MO);
		evals("004e", "~!@eq(false, false && true) @!~1;", STMT, MO);
		evals("004e", "~!@eq(false, true && false) @!~1;", STMT, MO);
		evals("004e", "~!@eq(false, false) @!~1;", STMT, MO);
		evals("004e", "~!@eq(false, false && true && true) @!~1;", STMT, MO);
		evals("004e", "~!@eq(false, true && false && true) @!~1;", STMT, MO);
		evals("004e", "~!@eq(false, true && true && false) @!~1;", STMT, MO);
	}
	
	public void test005_import_whole_module() throws Exception {
		evals("005a", "{import LO; ~!@eq(LO, LO)@!~;}", STMT, LO);
		evals("005a'", "{import OL=LO;  ~!@isdef(OL)@!~; ~!@undef(LO)@!~; }", STMT, LO);
		evals("005a'", "{import OL=LO; import LO; ~!@eq(LO, LO, OL)@!~;}", STMT, LO);
		evals("005b", "{import LO; ~!@eq(LO, LO)@!~;}", STMT, LO + VO);
		evals("005c", "{import LO; import LO; ~!@eq(LO, LO)@!~;}", STMT, LO + VO);
		evals("005d", "{import LO; import VO;  ~!@eq(LO, LO)@!~; ~!@eq(VO, VO)@!~;}", STMT, LO + VO);
		evals("005e", "{ {import LO; ~!@isdef(LO)@!~; } ~!@undef(LO)@!~;}", STMT, LO);
		evals("005f", "{import L1 = LO; import L2 = L1; ~!@isdef(L1)@!~; ~!@isdef(L2)@!~; ~!@undef(LO)@!~;}", STMT, LO);
		
		// Negative tests, to make sure that stuff from modules hasn't somehow gotten imported by mistake.
		evals("005-g", "{~!@undef(LO, la, lay, lie, lye, crash)@!~;}", STMT, LO);
		evals("005h", "{import L1 = LO; import L2 = L1; import L3 = L2; import lo = LO.la; import l1=L1.la; import l2 = L2.la; import l3 = L3.la; ~!@eq(lo,l1,l2,l3,'la')@!~; }", STMT, LO);
		
	}
	
	/*
	public static final String LO = "module LO{la = 'la'; lay = 'lay'; lie = 'lie'; lye = 'lye';} ";
	public static final String VO = "module VO{var vae; var vow; var view; var vo; var vent;} ";
	 */
	
	public void test006_import_member() throws Exception {
		evals("006a", "{import LO.la; ~!@eq(la, 'la')@!~;}", STMT, LO);
		evals("006a2", "{import al = LO.la; ~!@eq(al, 'la')@!~; ~!@undef(la)@!~; }", STMT, LO);
		evals("006b", "{import LO.la; import LO.lay; ~!@eq(la, 'la')@!~; ~!@eq(lay, 'lay')@!~;}", STMT, LO);
		evals("006c", "{import LO.la; import VO.vow; vow := la; ~!@eq(vow, la, 'la')@!~;}", STMT, LO + VO);
		evals("006c", "{import al = LO.la; import VO.vow; vow := al; ~!@eq(vow, al, 'la')@!~; ~!@undef(la)@!~;}", STMT, LO + VO);
		evals("006d", "{ {import LO.la; ~!@isdef(la)@!~; } ~!@undef(la)@!~;}", STMT, LO);
		evals("006d2", "{ {import al = LO.la; ~!@isdef(al)@!~; ~!@undef(la)@!~; } ~!@undef(al)@!~; ~!@undef(la)@!~;}", STMT, LO);
		evalFails("006e", "{LO = 1; import LO.la;}", STMT, LO, "but not of a module");
		
	}
	
	public void test007_import_star() throws Exception {
		evals("007a", "{import LO.*; ~!@eq(la, 'la')@!~;}", STMT, LO);
		evals("007a", "{import LO; import LO.*; ~!@eq(la, 'la')@!~;}", STMT, LO);
		evals("007a", "{import OL=LO; import LO.*; ~!@eq(la, 'la')@!~;}", STMT, LO);
		evals("007b", "{import LO.*; ~!@eq(la, 'la')@!~; lou = la; ~!@eq(lou, la, 'la')@!~;}", STMT, LO);
		evals("007c", "{import LO.*; import LO.*; ~!@eq(lay, 'lay')@!~;}", STMT, LO);
		evals("007d", "{import LO.*; {import LO.*; ~!@eq(lay, 'lay')@!~;} ~!@eq(lay, 'lay')@!~;}", STMT, LO);
		evals("007e", "{import LO.*; import VO.*; vo := lye; ~!@eq(vo, lye, 'lye')@!~;}", STMT, LO + VO);
		evals("007f", "{ {import LO.*; ~!@isdef(la)@!~; } ~!@undef(la)@!~;}", STMT, LO);
		evals("007g", "{import LO.*; {import LO.*; ~!@eq(la)@!~; } ~!@eq(la)@!~;}", STMT, LO);
		evals("007G", "{import LO.*; {import LO.*; ~!@isdef(la)@!~; } ~!@isdef(la)@!~;}", STMT, LO);
		evals("007h", "{import LO.la; {import LO.*; ~!@isdef(la)@!~; } ~!@isdef(la)@!~;}", STMT, LO);
		evals("007i", "{import OL = LO; import OL.*; ~!@isdef(la)@!~;~!@eq(la, 'la')@!~;}", STMT, LO);
	}
	
	public void test008_import_own() throws Exception {
		evals("008b", "{import own VAP = VO; import vaep = VAP.vae; vaep := 1; ~!@eq(vaep, 1)@!~; }", STMT, VO);
		evals("008a", "{import own VAP = VO; import own VOP = VO; ~!@ne(VAP, VOP)@!~; }", STMT, VO);
		evals("008c", "{import own V1 = VO; import v1 = V1.vae; import own V2 = VO; import v2 = V2.vae; v1 := 1; v2 := 2; ~!@ne(v1,v2)@!~; ~!@eq(v1,1)@!~; ~!@eq(v2,2)@!~;}", STMT, VO);
		evals("008e", "{import own V1 = VO; import v1 = V1.vae; import VO.vae; v1 := 1; vae := 2; ~!@ne(v1,vae)@!~;}", STMT, VO);
		evals("008f", "{import own V1 = VO; {import v1 = V1.vae; v1 := 1; ~!@eq(v1,1)@!~;} ~!@undef(v1)@!~; }", STMT, VO);
		evals("008f", "{import own V1 = VO; {import v1 = V1.vae; v1 := 1; ~!@eq(v1,1)@!~;} ~!@undef(v1)@!~; {import x = V1.vae; ~!@eq(x,1)@!~; x := 2; ~!@eq(x,2)@!~;} ~!@undef(x)@!~; {import y = V1.vae; ~!@eq(y,2)@!~;} ~!@undef(y)@!~;}", STMT, VO);
		evals("008d", "{import  V1 = VO; import v1 = V1.vae; import  V2 = VO; import v2 = V2.vae; v1 := 1; v2 := 2; ~!@eq(v1,v2)@!~;}", STMT, VO);
		evals("008g", "{import own V1 = VO; import Vo = VO; import VO; import own V2 = VO;  import v1ae = V1.vae; import Voae = Vo.vae; import v2ae = V2.vae; import VOae = VO.vae; v1ae := 1; v2ae := 2; Voae := 3; VOae := 4; ~!@eq(v1ae, 1)@!~; ~!@eq(v2ae, 2)@!~; ~!@eq(Voae, 4)@!~; ~!@eq(VOae, 4)@!~;  }", STMT, VO);
		evalFails("008h", "{import own VO;}", STMT, VO, "not one of the approved forms");
	}
	
	public void test009_fun() throws Exception {

		evals("009c", "{var v := 0; a = fn() = {v := v + 1; v;}; ~!@eq(a(), v, 1)@!~; ~!@eq(a(),v, 2)@!~;~!@eq(a(),v ,3)@!~;}", STMT, VO);
		evals("009a", "{a = fn(x)=x+1; b = a(1); ~!@eq(a,a)@!~; ~!@eq(b,2)@!~;}", STMT, VO);
		evals("009b", "{var v := fn (x) = x+1; ~!@eq(v(2), 3)@!~;}", STMT, VO);
		// functions with funs as args.
		evals("009d", "{var f; {var v := 0; f := fn()={v := v + 1; v;};} ~!@eq(f(), 1)@!~; ~!@undef(v)@!~; ~!@eq(f(), 2)@!~; }", STMT, VO);
		evals("009e", "{f = fn(g)=g(g(3)); inc = fn(x)=x+1; ~!@undef(g,x)@!~; ~!@eq(f(inc), 5)@!~; ~!@eq(f(fn(x)=x+x+x+x), 48)@!~;}", STMT, VO);
		evals("009f", "{tw = fn(f) = (fn(x) = f(f(x))); inc = fn(x)=x+1; ~!@eq(tw(inc)(3), 5)@!~; ~!@eq(tw(tw)(inc)(3), 7)@!~;~!@eq(tw(tw(tw))(inc)(3), 19)@!~;}", STMT, VO);
		// functions returning funs.
		// ... a function that produces a counter.
		evals("009g", "{mk = fn(){var c := 0; fn(){c := c+1; c;};}; a = mk(); b = mk(); ~!@eq(a(), 1)@!~; ~!@eq(a(), 2)@!~;~!@eq(b(), 1)@!~; ~!@eq(b(), 2)@!~;~!@eq(a(), 3)@!~;~!@eq(a(), 4)@!~;}", STMT, VO);

		
	}
	
	public void test010_fn_in_module() throws Exception {
		// functions in modules.	
		evals("010a", "{import PHO.inc; ~!@eq(inc(), 1)@!~; ~!@eq(inc(), 2)@!~;}", STMT, PHO);
		evals("010b", "{import own FAUX = PHO; import ink = FAUX.inc; import PHO.inc; ~!@eq(inc(), 1)@!~; ~!@eq(inc(), 2)@!~;~!@eq(ink(), 1)@!~;~!@eq(ink(), 2)@!~;~!@eq(inc(), 3)@!~;}", STMT, PHO);
		evals("010c",
		   "{import HOFO.*; ~!@eq(inc(2),3)@!~; ~!@eq(twice(inc)(2), 4)@!~; ~!@eq(twice(twice(inc))(2), 6)@!~; }"
				, STMT, HOFO);
	}
	
	public void test011_fn_with_simple_patterns() throws Exception {
		evals("011a", "{f = fn(1)=100 | x = (x+1); ~!@eq(f(1), 100)@!~; ~!@eq(f(2), 3)@!~; }", STMT, PHO);
		evals("011b", "{f = fn(1)=100 | (2)=200 | x = (x+1); ~!@eq(f(1), 100)@!~; ~!@eq(f(2), 200)@!~; ~!@eq(f(3),4)@!~; }", STMT, PHO);
		evals("011c", 
			"{f = fn(1,1)=101 | (1,2) = 102 | (2,1) = 201 | (2,2) = 202; " +//
			"~!@eq(f(1,1), 101)@!~; " +//
			"~!@eq(f(1,2), 102)@!~; " +//
			"~!@eq(f(2,1), 201)@!~; " +//
			"~!@eq(f(2,2), 202)@!~; " +//
			"}"
				, STMT, PHO
		);
	}
	
	
	
	public void test012_basic_fun () throws Exception {
		evals("012a", "{fun tw(x) = x+x; ~!@eq(tw(3), 6)@!~;}", STMT, PHO);
		evals("012b", "{fun f(1) = 11; | f(2) = 22; | f(x) = x+x; ~!@eq(f(1), 11)@!~; ~!@eq(f(2),22)@!~; ~!@eq(f(3),6)@!~; }", STMT, PHO);
		evals("012c", "{fun f(1) = 1; | f(x) = f(x - 1) + f(x - 1); ~!@eq(f(1), 1)@!~; ~!@eq(f(2), 2)@!~; ~!@eq(f(3), 4)@!~; ~!@eq(f(4), 8)@!~;}", STMT, PHO);
		evals("012d", "{fun f(x) = g(x) + 10; fun g(x) = x + 100; ~!@eq(f(1), 111)@!~;}", STMT, PHO);
		String CIRCE = "module CIRCE {fun f(x) = g(x) + 10; fun g(x) = x + 100;} ";
		evals("012e", "{import CIRCE.*; ~!@eq(f(1),111)@!~;}", STMT, CIRCE);
		String n = "\n";
		evals("012f", "{" +n+//
				"fun mfg(x){var v := x; " +n+//
				"   fun f(0){} | f(y){ v := v + y; f(y - 1); }" +n+//
				"   f(x - 1);" +n+//
				"   v;" +n+// 
				"}mfg" +n+//
				"~!@eq(mfg(1), 1)@!~;" +//
				"~!@eq(mfg(2), 3)@!~;" +//
				"~!@eq(mfg(3), 6)@!~;" +//
				"~!@eq(mfg(4), 10)@!~;" +//
				"}", 
				STMT, CIRCE);
		evalFiles("012h", "012-fun", CIRCE);
	}
	
	public void test013_math() throws Exception {
		evals("013a", "{~!@eq(4 - 3, 1)@!~;}", STMT, PHO);
		evalFiles("013b", "013-math", PHO);
	}

	private static void eqlist(String loc, ListTh list, Thing... els) throws Exception {
		assertEquals(loc + "/eqlist/=",  els.length, list.num());
		int i = 0;
		for (Thing thing : list) {
			Thing e = els[i];
			assertEquals(loc + "/eqlist/(" + i + ")", e, thing);
			i++;
		}
	}
	
	
	
	
	public void test014_list() throws Exception {
		evals("014-sneaky-b", "{L = [-1,0,1,2,3]; M = L.sub(1, -1); ~!@eq(M, [0,1,2,3])@!~; N = M.sub(0,2); ~!@eq(N, [0,1,2])@!~;}", STMT, MO);
		evals("014c", "{L = [1,2,3]; [a,b,c,d...] = L; ~!@eq(a,1)@!~; ~!@eq(b,2)@!~; ~!@eq(c,3)@!~; ~!@eq(d, [])@!~;}", STMT, MO);
		evalFiles("014a", "014-list", PHO);
	}
	
	static final public String DUP_DEF = "Duplicate definition"; 
	public void test015_class_basics() throws Exception {
		evalFiles("015a", "015-class", CLO);
		evalFails("015b", "{var v := 1; class C{ var v := 2; } }", STMT,  CLO, DUP_DEF);
		evalFails("05c", "{class C { var a := 1; val a = 1; }}", STMT, CLO, DUP_DEF);
//		evalFails("05c", "{class C { var a := 1; method a()=a; }}", STMT, CLO, DUP_DEF);
	}
	
	public void test016_class_and_module_fun() throws Exception {
		String modules = Bard.contentsOf(TestUtils.testfile("eval/016-class/Modules.thm"));
		evalFiles("016a", "016-class", modules);
	}

	public void test016alt_valof() throws Exception {
		evals("016alt", "valof{}", EXP, CLO);
		evals("016blt", "valof{~!@hit()@!~; }", EXP, CLO);
		evals("016clt", "~!@eq(valof{~!@hit()@!~; var x := 1; x := x + 1; x + 1;}, 3)@!~", EXP, CLO);
	}

	public void test017_loops_and_control_flow() throws Exception {
		String modules = Bard.contentsOf(TestUtils.testfile("eval/017-loop/Modules.thm"));
		evalFiles("017", "017-loop", modules);
	}
	
	
	public void test018_comparison() throws Exception {
		evals("018a", "~!@eq(1<2, true)@!~;", STMT, CLO);
		evals("018a1", "~!@eq(null≤1, true)@!~;", STMT, CLO);
		evals("018a1", "~!@eq(1≤null, false)@!~;", STMT, CLO);
		String modules = Bard.contentsOf(TestUtils.testfile("eval/018-compare/Modules.thm"));
		evalFiles("018b", "018-compare", modules);
	}
	
	public void test019_if() throws Exception {
		String modules = Bard.contentsOf(TestUtils.testfile("eval/019-if/Modules.thm"));
		evalFiles("019", "019-if", modules);
		evalFails("019b", "{if (x = 1) {2;}}", STMT, VO, "ParseException");
	}
	
	
	public void test020_match_simple_list() throws Exception {
		String modules = Bard.contentsOf(TestUtils.testfile("eval/020-match-list/Modules.thm"));
		evalFiles("020", "020-match-list", modules);
	}
	
	public void test021_for() throws Exception {
		String modules = Bard.contentsOf(TestUtils.testfile("eval/021-for/Modules.thm"));
		evalFiles("021", "021-for", modules);
	}
	
	public void test022_assorted_minor_crap() throws Exception {
		evals("022a", "{null < 1; ~!@hit()@!~;}", STMT, CLO);
		evals("022a", "{null < null; ~!@hit()@!~;}", STMT, CLO);
		evals("022b", "{var a := 1; var b := 2; ~!@eq(a,1)@!~; ~!@eq(b,2)@!~; a,b := b,a; ~!@eq(a,2)@!~; ~!@eq(b,1)@!~; }", STMT, CLO);
		evals("022b", "{var a := 1; var b := 2; var c := 3;~!@eq(a,1)@!~; ~!@eq(b,2)@!~; ~!@eq(c,3)@!~; a,b,c := b,c,a; ~!@eq(a,2)@!~; ~!@eq(b,3)@!~; ~!@eq(c,1)@!~; }", STMT, CLO);
		evals("022e", "{import VO.*; var a := 1; vae := 2; ~!@eq(a,1)@!~; ~!@eq(vae,2)@!~; a,vae := vae, a; ~!@eq(a,2)@!~; ~!@eq(vae, 1)@!~; }" , STMT, VO);
	}
	
	public void test023_patterns() throws Exception {
		evals("023a", "{if (1 ~ (x && y)) {~!@eq(x,1)@!~; ~!@eq(y,1)@!~;}}", STMT, VO);
		evals("023b", "{if (1 ~ (x && 2)) {~!@hit()@!~0;} else {~!@undef(x)@!~;}}", STMT, VO);
		String modules = Bard.contentsOf(TestUtils.testfile("eval/023-match/Modules.thm"));
		evalFiles("023", "023-match", modules);
	}
	
	public void test024_anon_obj() throws Exception {
		String modules = Bard.contentsOf(TestUtils.testfile("eval/024-anon-obj/Modules.thm"));
		evalFiles("024", "024-anon-obj", modules);
	}
	
	
	public void test025_match() throws Exception {
		String modules = Bard.contentsOf(TestUtils.testfile("eval/025-match/Modules.thm"));
		evalFiles("025", "025-match", modules);
	}
	
	public void test026_match() throws Exception {
		String modules = Bard.contentsOf(TestUtils.testfile("eval/026-return/Modules.thm"));
		evalFiles("026", "026-return", modules);
	}
	
	public void test027_try() throws Exception {
		String modules = Bard.contentsOf(TestUtils.testfile("eval/027-try/Modules.thm"));
		evalFiles("027", "027-try", modules);
	}
	
	public void test028_record() throws Exception {
		String modules = Bard.contentsOf(TestUtils.testfile("eval/028-records/Modules.thm"));
		evalFiles("028", "028-records", modules);
		evals("02ab", "{j = 1; r = {: j :}; ~!@eq(r.j, j, 1)@!~;}", STMT, MO);
	}
	
	
	public void test029_superclass() throws Exception {
		evals("029a", "{class A{a = 1;}  class B extends A{b = this.a+1;}  be = B(); ~!@eq(be.a, 1)@!~; ~!@eq(be.b, 2)@!~; }", STMT, MO);
		evals("029a", "{class A{a = 1;}  class B extends A{b = 2;}  be = B(); ~!@eq(be.a, 1)@!~; ~!@eq(be.b, 2)@!~; }", STMT, MO);
		evals("029c", "{class A{a=1;} class B{b=2;} class C extends A,B {c=this.a+this.b;}  c=C(); ~!@eq(c.a, 1)@!~; ~!@eq(c.b,2)@!~; ~!@eq(c.c, 3)@!~;}", STMT, MO);
		String modules = Bard.contentsOf(TestUtils.testfile("eval/029-simple-superclass/Modules.thm"));
		evalFiles("029", "029-simple-superclass", modules);
		
	}
	
	public void test030ModNCls() throws Exception {
		String modules = Bard.contentsOf(TestUtils.testfile("eval/030-mod+cls/Modules.thm"));
		evalFiles("030", "030-mod+cls", modules);
	}
	
	public void test031_ctor() throws Exception {
		
		evals("031~a", "{class C{var v; new C(x){v := x;}}; cc = C(100); ~!@eq(cc.v, 100)@!~;}", STMT, MO);
		evals("031~b", "{class C{ val v; new C(x) { v = x; }}class  c = C(1); ~!@eq(c.v, 1)@!~;}", STMT, MO);
		evalFails("031~c", "{class C{val v; new C(x) { if (true) {v = x;}}}class }", STMT, MO, "");
		evalFails("031~d", "{class C{val v; new C(x) { v := x;}}class }", STMT, MO, "");
		
		String dir = "031-ctor";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("031", dir, modules);
	}
	
	public void test032_string_interp() throws Exception {
		String dir = "032-string-interp";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("032", dir, modules);
	}
	
	public void test033_supercall() throws Exception {
		evalFails("zzz", "{class A{def f()=1; def g()=this.gf()+1;} ~!@eq(A().g(), 2)@!~;}", STMT, MO, "No method named gf");
		String dir = "033-supercall";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("033", dir, modules);
		evalFails("033-h", "{import SIN.Date;}", STMT, modules, "No definition for Date in SIN");
		}
	
	
	
	public void test034_new_super() throws Exception {
		String dir = "034-new-super";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("034", dir, modules);
	}
	
	public void test035_ranges() throws Exception {
		evals("035a", "{r = 0..2; for (i <- r) {~!@hit()@!~3;}}", STMT, MO);
		String dir = "035-range";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("035", dir, modules);
	}
	
	public void test036_some() throws Exception {
		evals("036a", "{a = %some(x == 1 | for x <- [2,1,3]); ~!@eq(a,true)@!~;}", STMT, MO);
		String dir = "036-quantifiers";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("036", dir, modules);
		
	}
	
	
	public void test037_query() throws Exception {
		String dir = "037-query";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("037", dir, modules);
	}
	
	public void test038_meth_on_builtin() throws Exception {
		String dir = "038-meth-on-builtin";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("038", dir, modules);
		
	}
	
	
	public void test039_float() throws Exception {
		String dir = "039-float";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("039", dir, modules);
		
	}
	
	
	public void test040_table() throws Exception {
		String dir = "040-table";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("040", dir, modules);
		
	}
	
	
	
	public void test041_null() throws Exception {
		String dir = "041-null";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("041", dir, modules);
		
	}
	
	
	public void test042_it() throws Exception {
		String dir = "042-it";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("042", dir, modules);
		
	}
	
	
	public void test043_class_formals() throws Exception {
		String dir = "043-class-formals";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("043", dir, modules);
		
	}
	
	
	public void test044_map() throws Exception {
		String dir = "044-map";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("044", dir, modules);
		
	}
	
	public void test045_ord() throws Exception {
		String dir = "045-ord";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("045", dir, modules);
		
	}
	
	public void test046_group() throws Exception {
		String dir = "046-group";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("046", dir, modules);
	}
	
	
	
	public void test047_file() throws Exception {
		String dir = "047-file";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("047", dir, modules);
	}

	public void test048_id() throws Exception {
		String dir = "048-id";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("048", dir, modules);
		
	}
	
	public void test050_parenstmt() throws Exception {
		String dir = "050-parenstmt";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("050", dir, modules);
		
	}
	
	public void test051_maprec() throws Exception {
		String dir = "051-maprec";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("051", dir, modules);
		
	}
	
	public void test053_javaly() throws Exception {
		String dir = "053-extend";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("053", dir, modules);
	}
	
	public void test054_pure() throws Exception {
		String dir = "054-pure";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("054", dir, modules);
	}
	
	public void test055_xml() throws Exception {
		String dir = "055-xml";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("055", dir, modules);
	}
	
	public void test056_type_constraints() throws Exception {
		String dir = "056-type-constraint";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("056", dir, modules);
	}
	
	public void test057_smalltalky() throws Exception {
		String dir = "057-small";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("057", dir, modules);
	}
	
	public void test058_clsvar() throws Exception {
		String dir = "058-clsvar";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("058", dir, modules);
	}
	
	
	public void test059_oopsla() throws Exception {
		String dir = "059-oopsla";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("059", dir, modules);
	}
	
	

	public void test060_misc() throws Exception {
		String dir = "060-misc";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("060", dir, modules);
	}
	
	
	public void test061_sandbox() throws Exception {
		String dir = "061-sandbox";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("061", dir, modules);
	}
	
	
	
	public void test062_identity() throws Exception {
		String dir = "062-identity";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("062", dir, modules);
	}
	
	
	public void test063_stupid_module() throws Exception {
		String dir = "063-stupid-module";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("063", dir, modules);
	}
	
	public void test064() throws Exception {
		String dir = "064-mod-priv";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("064", dir, modules);
	}
	
	public void test065_evil_diamonds() throws Exception {
		String dir = "065-evil-diamonds";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("065", dir, modules);
	}
	
	public void test066_bytes() throws Exception {
		String dir = "066-bytes";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("066", dir, modules);
	}
	
	public void test067_char() throws Exception {
		String dir = "067-char";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("067", dir, modules);
	}
	public void test068_invoke() throws Exception {
		String dir = "068-invoke";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("068", dir, modules);
	}
	public void test069_privatization() throws Exception {
		String dir = "069-privatization";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("069", dir, modules);
	}
	public void test070_module_bug() throws Exception {
		String dir = "070-module-bug";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("070", dir, modules);
	}
	public void test071_json() throws Exception {
		String dir = "071-json";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("071", dir, modules);
	}
	public void test072_math() throws Exception {
		String dir = "072-math";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("072", dir, modules);
	}
	
	public void test073_security() throws Exception {
		String dir = "073-security";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("073", dir, modules);
	}
	
	
	public void testx02() throws Exception {
		String modules = Bard.contentsOf(TestUtils.testfile("eval/x02/Modules.thm"));
		evalFiles("x02", "x02", modules);
	}
	


	public void testx03_text() throws Exception {
		String dir = "x03-text";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("x03", dir, modules);
		
	}
	


	public void testx04_pl_day_2009() throws Exception {
		String dir = "x04-pl-day-2009";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("x04", dir, modules);
	}
	
	
	public void testx05_topsort() throws Exception {
		String dir = "x05-topsort";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("x05", dir, modules);
	}
	public void testx06_recipe() throws Exception {
		String dir = "x06-recipe";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("x06", dir, modules);
	}
	public void testx07_html() throws Exception {
		String dir = "x07-html";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("x07", dir, modules);
	}
	
	public void testx09_capabilities() throws Exception {
		String dir = "x09-capabilities";
		String modules = Bard.contentsOf(TestUtils.testfile("eval/" + dir + "/Modules.thm"));
		evalFiles("x09", dir, modules);
	}
	
	public void KNOWN_FAILURES() throws Exception {
		evals("022e", "{import VO; var a := 1; VO.vae := 2; ~!@eq(a,1)@!~; ~!@eq(VO.vae,2)@!~; a,VO.vae := VO.vae, a; ~!@eq(a,2)@!~; ~!@eq(VO.vae, 1)@!~; }" , STMT, VO);

	}

	
}
