
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

import java.util.List;

import sun.net.www.ParseUtil;
import static fisher.parser.SyntacticClass.*;
import fisher.parser.SyntacticClass;
import fisher.statics.purity.PurityStatus;
import fisher.statics.purity.StaticPurityChecker;
import fisher.syn.AnonObj;
import fisher.syn.ClsDecl;
import fisher.syn.FunDecl;
import fisher.syn.MethDecl;
import fisher.syn.core.Syntax;
import fisher.syn.interfaces.Puretic;
import fisher.util.Bard;
import fisher.util.Compilation;
import junit.framework.TestCase;
import static fisher.statics.purity.PurityStatus.*;

public  class  PurityTest extends TestCase  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public static Puretic compileThroughPurityCheck(String loc, String subject, SyntacticClass cls,  Class CL, int num, String moduleCode)
			throws Exception {
		TestUtils.reset(loc);
		Syntax syn = SealTest.sealize(loc, subject, cls, moduleCode);
		Puretic pur = null;
		List<Syntax> desc = syn.computeDescendants();
		int ncl = 0;
		for (Syntax syntax : desc) {
			if (CL.isInstance(syntax)) {
				ncl++;
				if (ncl == num) {
					pur = (Puretic) syntax;
				}
			}
		}
		// Purity-check everything.
		for (Syntax syntax2 : desc) {
			if (syntax2 instanceof ClsDecl || syntax2 instanceof FunDecl || syntax2 instanceof AnonObj) {
				Puretic purpur = (Puretic) syntax2;
				StaticPurityChecker.check(purpur);
			}
		}
		assertNotNull(loc + "/nonnull", pur);
		// No longer needed: StaticPurityChecker.check(pur);
		return pur;
	}

	public static void pure(String loc, String code, fisher.parser.SyntacticClass cls, Class CL, int i, String moduleCode)
	throws Exception {
		Puretic pur = compileThroughPurityCheck(loc, code, cls, CL, i, moduleCode);
		assertTrue(loc + "/msg\n" + Bard.sep(Compilation.current.messages, "\n\n"), Compilation.current.messages.isEmpty());
		PurityStatus purityStatus = pur.purityStatus();
		assertTrue(loc + "/pure: " + purityStatus, purityStatus == PURE);
	}
	
	public static void innocent(String loc, String code, fisher.parser.SyntacticClass cls, Class CL, int pos, String moduleCode)
			throws Exception {
		Puretic pur = compileThroughPurityCheck(loc, code, cls,CL,  pos, moduleCode);
		assertTrue(loc + "/msg", Compilation.current.messages.isEmpty());
		assertTrue(loc + "/innocent", pur.purityStatus() == INNOCENT);
	}

	public static void impure(String loc, String code, SyntacticClass cls, Class CL, int pos, String moduleCode) 
			throws Exception {
		Puretic pur = compileThroughPurityCheck(loc, code, cls, CL, pos, moduleCode);
		PurityStatus purityStatus = pur.purityStatus();
		assertTrue(loc + "/impure -- was " + purityStatus, purityStatus == IMPURE);
	}
	
	
	public static void purityError(String loc, String code, SyntacticClass cls, Class CL, int pos, String moduleCode, String... msgs) throws Exception {
		Puretic pur = compileThroughPurityCheck(loc, code, cls,CL,  pos, moduleCode);
		if (ERROR != pur.purityStatus()) {
			assertEquals(loc + "/wants-purity-error", ERROR, pur.purityStatus());
		}
		if (msgs.length > 0) {
			if (Compilation.current.messages.isEmpty()) {
				assertFalse(loc + "/impure/non-fail -- this should have had errors, but didn't.", true);
			}
			String glomMessages = Bard.sep(Compilation.current.messages, "\n\n");
			for (String string : msgs) {
				if (!(glomMessages.contains(string))) {
					assertFalse("The following obligatory error message is missing: \n" + string
							+ "\n\nThe messages are: \n" + glomMessages + "\n", true);
				}
			}
		}
	}
	
	public void testp00() throws Exception {
	}

	public void testp01_isThisThingOn() throws Exception {
		innocent("p01a", "class C{}", CLS, ClsDecl.class, 1, "");
		pure("p01b", "class C:pure{}", CLS, ClsDecl.class, 1, "");
		impure("p01c", "class C{var x;}", CLS, ClsDecl.class, 1, "");
	}
	
	public static final String MO = "module MO {class Inno{} class Puro:pure{} class Evil{var x;}}";
	
	public void testp02_classes() throws Exception {
		pure("p02a", "{import MO.*; class P2 : pure extends Puro{}}", STMT, ClsDecl.class, 1, MO);
		pure("p02b", "{import MO.*; class P2 : pure extends Inno{}}", STMT, ClsDecl.class, 1, MO);
		purityError("p02c", "{import MO.*; class P2 : pure extends Evil{}}", STMT, ClsDecl.class, 1, MO);
		innocent("p02d", "{import MO.*; class I2 extends Puro{}}", STMT, ClsDecl.class, 1, MO);
		innocent("p02e", "{import MO.*; class I2 extends Inno{}}", STMT, ClsDecl.class, 1, MO);
		impure("p02f", "{import MO.*; class I2 extends Evil{}}", STMT, ClsDecl.class, 1, MO);
		impure("p02g", "{import MO.*; class I2 extends Evil{ var y;}}", STMT, ClsDecl.class, 1, MO);
		impure("p02g", "{import MO.*; class I2 extends Puro{ var y;}}", STMT, ClsDecl.class, 1, MO);
		impure("p02g", "{import MO.*; class I2 extends Inno{ var y;}}", STMT, ClsDecl.class, 1, MO);
	}
	
	public void testp03_methods() throws Exception {
		pure("p03a", "class C:pure{ def m():pure{}}", STMT, MethDecl.class, 1, MO);
		innocent("p03b", "class C:pure{ def m(){}}", STMT, MethDecl.class, 1, MO);
		pure("p03c", "class C:pure{ def m():pure{var v; v := 1; v;}}", STMT, MethDecl.class, 1, MO);
		impure("p03d", "{var x; class C{ def m()=x; }} ", STMT, MethDecl.class, 1, MO);
		purityError("p03e", "{var x; class C:pure{ def m()=x; }} ", STMT, ClsDecl.class, 1, MO);
		purityError("p03f", "{import MO.*; var x; class C:pure{ def m()=Evil(); }} ", STMT, ClsDecl.class, 1, MO);
		pure("p03g", "{import MO.*; var x; class C:pure{ def m()=Puro(); }} ", STMT, ClsDecl.class, 1, MO);
		pure("p03h", "{import MO.*; var x; class C:pure{ def m()=Inno(); }} ", STMT, ClsDecl.class, 1, MO);
		
		impure("p03f", "{import MO.*; var x; class C{ def m()=Evil(); }} ", STMT, ClsDecl.class, 1, MO);
		innocent("p03g", "{import MO.*; var x; class C{ def m()=Puro(); }} ", STMT, ClsDecl.class, 1, MO);
		innocent("p03h", "{import MO.*; var x; class C{ def m()=Inno(); }} ", STMT, ClsDecl.class, 1, MO);
		impure("p03i", "{class C:pure{def m(){var i; fn()=i;}}}", STMT, MethDecl.class, 1, MO);
		purityError("p03j", "{class C:pure{def m(){var i; fn()=i;}}}", STMT, ClsDecl.class, 1, MO);
	}
	
	
	public static final String SN = "module SN{ var v; class Yck { def myck()=v; }; class Yok { def m() = (fn () = v); } }";
	public void testp04_snicking_stuff_into_methods() throws Exception {
		purityError("p04a", "{import SN.*; class C:pure extends Yck{}}", STMT, ClsDecl.class, 1, SN);
		purityError("p04b", "{import SN.*; class C:pure extends Yok{}}", STMT, ClsDecl.class, 1, SN);
		purityError("p04c", "{import SN.*; class Zok extends Yok{} class C:pure extends Zok{}}", STMT, ClsDecl.class, 2, SN);
		purityError("p04d", "{var v; class Yurd:pure{w = (fn () = v);}}", STMT, ClsDecl.class, 1, SN);	
		purityError("p04e", "{var v; class Yelt:pure{val jug; new Yelt(){jug = v;}}}", STMT, ClsDecl.class, 1, SN);
		pure("p04f", "{var v; class Yelt:pure{val jug; new Yelt(w){jug = w;}} tley = Yelt(v);}", STMT, ClsDecl.class, 1, SN);
		innocent("p04g", "{var v; class Yelt{val jug; new Yelt(w){jug = w;}} tley = Yelt(v);}", STMT, ClsDecl.class, 1, SN);
	}
	
	public void testp05_too_strict() throws Exception {
		purityError("p05a", "{var v; class Yurd:pure{w = v;}}", STMT, ClsDecl.class, 1, SN);
		purityError("p05b", "{var v; object:pure{w = v;};}", STMT, AnonObj.class, 1, SN);
	}
	
	
	public void testp06_class_with_methods() throws Exception {
		innocent("p07a", "class C(x,y) : pure {def sum() = x+y; def diff()=x-y;}", CLS, MethDecl.class, 1, "");
		pure("p07b", "class C(x,y) : pure {def sum() = x+y; def diff()=x-y;}", CLS, ClsDecl.class, 1, "");
	}
	
	public void testp07_tobias_1() throws Exception {
		pure("p07a", "class Good: pure { " + //  
					"val goodVal1 : pure = 57;" + // 
					"val goodVal2 = 57; /* pure should be implicit in pure classes */ " + //
					"def goodFun1(x) = x.bad(); " +//
					"/* no var arguments! def goodFun2(var x) = x.bad(); */" +//
					"def goodFun3(x : pure  ): pure = x.bad(); " +//
					"}",
					CLS, ClsDecl.class, 1, SN);
		// Tobias called this one Impure, but that's only because of the annotation, which I don't support yet.  So it's innocent
		innocent("p07b", "class Impure(/* var */ f /*: Impure*/) { def purelyBehavingFun() = 42; }", CLS, MethDecl.class, 1, "");
		//innocent("p07c", "{fun food(x):pure { x.whatever() := 1; }}", STMT, FunDecl.class, 1, "" );
		purityError("p07d", "class Bad2:pure {var badVar;}", STMT, ClsDecl.class, 1, "");
		purityError("p07e", "class Bad5:pure {var badly; def bad3() { badly:=1;} }", STMT, ClsDecl.class, 1, "");
		purityError("p07f", "{var badly; class Bad6:pure{ def bad3() {badly:=1;}}}", STMT, ClsDecl.class, 1, "");
		pure("p07g", "class Bad3:pure {def fummeth(x) {x.f();}}", STMT, ClsDecl.class, 1, "");
//		purityError("p07z", "class Bad3:pure {def assmeth(x) {x.f := 10;}}", STMT, ClsDecl.class, 1, "");
	}
	
//	public void testp08_superclasses() throws Exception {
//		String NO = "";
//		String nl = "\n";
////		pure("p08a", 
////				"{" +nl+//
////				"  class A:pure{val x; new A(xx) {x=xx;}}" +nl+//
////				"  class B:pure extends A{" +nl+//
////				"     new B(xx,yy) { new@A(xx); y = yy; }" +nl+//
////				" }B" +nl+//
////				"}"
////				,STMT, ClsDecl.class, 1, NO
////		);
//		pure("p08b", 
//				"{" +nl+//
//				"  class A:pure{val x; new A(xx) {x=xx;}}" +nl+//
//				"  class B:pure extends A{val y;" +nl+//
//				"     new B(xx,yy) { new@A(xx); y = yy + x; }" +nl+//
//				" }B" +nl+//
//				"}"
//				,STMT, ClsDecl.class, 1, NO
//		);
//	}
	
	public void testp09_for_oopsla() throws Exception {
		String nl = "\n";
		String code = "" + nl+//
			"class C:pure{"  +nl+//
			"def sum3np1(f, nn) { var sum := 0; var n := nn;" +nl+//
			"  while (n != 1) { sum += f(n);" + nl+ //
			"    if (n mod 2 == 0) n := n div 2; else n := 3*n + 1;" + nl + //
			"  }}}"
		;
			
			
		pure("p09a",code, STMT, ClsDecl.class, 1, "");
		
		
	}
	
	
	
	
}
