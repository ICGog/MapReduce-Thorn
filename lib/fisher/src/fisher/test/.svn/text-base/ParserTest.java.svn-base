
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

import static fisher.parser.SyntacticClass.CLS;
import static fisher.parser.SyntacticClass.EXP;
import static fisher.parser.SyntacticClass.MODU;
import static fisher.parser.SyntacticClass.PAT;
import static fisher.parser.SyntacticClass.STMT;
import static fisher.syn.core.ComparisonOp.GE;
import static fisher.syn.core.ComparisonOp.GT;
import static fisher.syn.core.ComparisonOp.IN;
import static fisher.syn.core.ComparisonOp.LE;
import static fisher.syn.core.ComparisonOp.LT;
import static fisher.syn.core.ComparisonOp.NE;
import static fisher.syn.core.Op.ADDTO;
import static fisher.syn.core.Op.AND;
import static fisher.syn.core.Op.APPEND;
import static fisher.syn.core.Op.CONS;
import static fisher.syn.core.Op.DELFROM;
import static fisher.syn.core.Op.DOTDOT;
import static fisher.syn.core.Op.DOTDOTLT;
import static fisher.syn.core.Op.FDIV;
import static fisher.syn.core.Op.IDIV;
import static fisher.syn.core.Op.MOD;
import static fisher.syn.core.Op.ONEOF;
import static fisher.syn.core.Op.OR;
import static fisher.syn.core.Op.PLUS;
import static fisher.syn.core.Op.TIMES;
import static fisher.syn.core.OpAB.FDIVAB;
import static fisher.syn.core.OpAB.MINUSAB;
import static fisher.syn.core.OpAB.PLUSAB;
import static fisher.syn.core.OpAB.TIMESAB;
import static fisher.syn.core.SortOrder.ASCENDING;
import static fisher.syn.core.SortOrder.DESCENDING;
import static fisher.test.TestUtils.explain;
import static fisher.util.Bard.list;
import static fisher.util.Bard.sep;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.sun.org.apache.bcel.internal.generic.IDIV;

import junit.framework.TestCase;
import fisher.parser.ParseException;
import fisher.parser.SyntacticClass;
import fisher.syn.Alias;
import fisher.syn.AnonFun;
import fisher.syn.AnonObj;
import fisher.syn.AssignToSubscripted;
import fisher.syn.AsyncDecl;
import fisher.syn.AsyncStmt;
import fisher.syn.BracketCall;
import fisher.syn.Break;
import fisher.syn.Case;
import fisher.syn.ClassFormal;
import fisher.syn.ClsDecl;
import fisher.syn.ClsPatDef;
import fisher.syn.Continue;
import fisher.syn.For;
import fisher.syn.FunBody;
import fisher.syn.FunDecl;
import fisher.syn.If;
import fisher.syn.ImportStmt;
import fisher.syn.ItExp;
import fisher.syn.ListForGroup;
import fisher.syn.MapCtor;
import fisher.syn.MatchExp;
import fisher.syn.MethDecl;
import fisher.syn.ModArgBinding;
import fisher.syn.Module;
import fisher.syn.ModuleFileAlias;
import fisher.syn.ModuleFileImport;
import fisher.syn.ModuleFileMemberStmt;
import fisher.syn.ModuleFileVisibility;
import fisher.syn.MonoBody;
import fisher.syn.OpABExp;
import fisher.syn.PatAnd;
import fisher.syn.PatEvalTestExp;
import fisher.syn.PatExtract;
import fisher.syn.PatInterpolation;
import fisher.syn.PatMatchSomethingElse;
import fisher.syn.PatNot;
import fisher.syn.PatNotNull;
import fisher.syn.PatOr;
import fisher.syn.PatRecordCtor;
import fisher.syn.PatRecordField;
import fisher.syn.PatTypeTest;
import fisher.syn.PatWildcard;
import fisher.syn.Probe;
import fisher.syn.ProcBody;
import fisher.syn.ProcInit;
import fisher.syn.QueryAfter;
import fisher.syn.QueryControlFor;
import fisher.syn.QueryControlIf;
import fisher.syn.QueryControlVal;
import fisher.syn.QueryControlVar;
import fisher.syn.QueryControlWhile;
import fisher.syn.QueryFirst;
import fisher.syn.QueryListComprehension;
import fisher.syn.QueryQuantifierCount;
import fisher.syn.QueryQuantifierEvery;
import fisher.syn.QueryQuantifierSome;
import fisher.syn.QuerySort;
import fisher.syn.QuerySwiss;
import fisher.syn.RecordCall;
import fisher.syn.Recv;
import fisher.syn.Send;
import fisher.syn.Serve;
import fisher.syn.ServeBlock;
import fisher.syn.SortKey;
import fisher.syn.Spawn;
import fisher.syn.StringBitText;
import fisher.syn.StringBitVar;
import fisher.syn.StringWithInterpolations;
import fisher.syn.SuperCall;
import fisher.syn.SuperCtorCall;
import fisher.syn.SyncDecl;
import fisher.syn.SyncStmt;
import fisher.syn.Throw;
import fisher.syn.TypeConstraint;
import fisher.syn.TypeConstraints;
import fisher.syn.Valof;
import fisher.syn.VarDecl;
import fisher.syn.While;
import fisher.syn.core.ComparisonOp;
import fisher.syn.core.Op;
import fisher.syn.core.OpAB;
import fisher.syn.core.QuoteStyle;
import fisher.syn.core.SortOrder;
import fisher.syn.core.Syntax;
import fisher.syn.core.Visibility;
import fisher.syn.interfaces.Puretic;
import fisher.util.Bard;
import fisher.util.FisherSource;

public  class  ParserTest extends TestCase  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	private void parsesAs(String loc, String subject, SyntacticClass cls, String desired) throws Exception {

		try {
			Syntax syn = TestUtils.parse(loc, subject, cls);
			assertEquals(explain(loc, subject, "parsesAs"), desired, syn.details());
			TestUtils.checkParentPointers(loc, syn);
		} catch (ParseException pe) {
			throw new Exception(loc + "\nsubject=" + subject + "\ndesired=" + desired + pe.toString(), pe);

		}
	}

	private void taut(String loc, String subject, SyntacticClass cls) throws Exception {
		// Taut tests check for tautonyms: 
		// a string which, when parsed and tostrung, yeilds itself.
		// This is a test for the tostring system.
		try {
			Syntax.doingStmt = (cls == STMT);
			Syntax syn = TestUtils.parse(loc, subject, cls);
			String after = syn.toString();
			String msg = explain(loc, subject, "taut") + "\n The subject's tostring should be the same as the subject."
					+ "\nsubject  = " + subject + "\ntostring = " + after + "\n\n";
			assertEquals(msg, subject, after);
		} catch (ParseException pe) {
			throw new Exception(loc + "/taut" + "\nsubject=" + subject + pe.toString(), pe);
		}
	}

	public static Syntax parses(String loc, String subject, SyntacticClass cls) throws Exception {

		try {
			Syntax syn = TestUtils.parse(loc, subject, cls);
			return syn;
		} catch (ParseException e) {
			throw e;
		}
	}

	private void fileMustParse(String loc, String filename) throws Exception {
		File file = TestUtils.testfile("parser/" + filename);
		//		System.out.println("fmp-file=" + file);
		if (file.isDirectory()) {
			String[] fns = file.list();
			for (String string : fns) {
				String subfile = filename + "/" + string;
				if (string.endsWith(".th")) {
					//					System.out.println("fmp-.th" + string);
					fileMustParse(loc + "/" + string, subfile);
				} else if (string.startsWith(".")) {
					// ignore
				} else if (string.endsWith("~")) {
					// ignore
				} else {
					fileMustParse(loc + "/" + string, subfile);
				}
			}
		} else {
			if (!file.canRead())
				return;
			String contents = Bard.contentsOf(file);
			SyntacticClass cls = STMT;
			if (contents == null) {
				throw new Exception(loc + "/file=" + file + " -- test file is empty!");
			}
			if (contents.startsWith("//CLS"))
				cls = CLS;
			if (contents.startsWith("//EXP"))
				cls = EXP;
			if (contents.startsWith("//MODS"))
				cls = SyntacticClass.MODUS;
			if (contents.startsWith("//MODU"))
				cls = MODU;
			parses(loc, contents, cls);
		}
	}

	private void doesntParse(String loc, String subject, SyntacticClass cls, String contains) throws Exception {

		try {
			TestUtils.parse(loc, subject, cls);
			throw new Exception(explain(loc, subject, "doesntParse") + " -- Not supposed to parse, but it did");
		} catch (ParseException e) {
			{
				String s = e.toString();
				if (s.contains(contains)) {
					// all is well!
					;
				} else {
					throw new Exception(explain(loc, subject, "doesntParse")
							+ " -- Failed to parse, but did so the wrong way. \n" + "It was supposed to contain \""
							+ contains + "\", but did not.\n" + "The actual exception text was: \n" + s
							+ "\n=======================================\n\n");
				}

			}
		}
	}

	private final static String NOTPAT = "Can't convert this expression to pattern";

	public void test001_literal() throws Exception {
		parses("001", "1", EXP);
		parsesAs("001a", "1", EXP, "(1)");
		taut("001", "1", EXP);
	}

	public static String IF(String te, String th, String el) {
		return "If(test=" + te + " Then=" + th + " Else=" + el + " un=false isExp=false)";
	}

	public static String UNLESS(String te, String th, String el) {
		return "If(test=" + te + " Then=" + th + " Else=" + el + " un=true isExp=false)";
	}

	public void test002_if() throws Exception {
		// If(test= Then= Else= un=false)
		parsesAs("002a", "if (1) 2; else 3;", STMT, "If(test=(1) Then=(2) Else=(3) un=false isExp=false)");
		parsesAs("002a", "if (1) 2; else 3;", STMT, IF("(1)", "(2)", "(3)"));
		parsesAs("002a", "unless (1) 2; else 3;", STMT, UNLESS("(1)", "(2)", "(3)"));
		parsesAs("002a-", "unless (1) 2; else 3;", STMT, "If(test=(1) Then=(2) Else=(3) un=true isExp=false)");
		parsesAs("002b", "if (1) 2; else if (3) 4; else 5;", STMT,
				"If(test=(1) Then=(2) Else=If(test=(3) Then=(4) Else=(5) un=false isExp=false) un=false isExp=false)");
		parsesAs("002c", "if (1) 2;", STMT, "If(test=(1) Then=(2) Else=null un=false isExp=false)");
		// Dangling else time
		parsesAs("002c", "if (1) if (2) 3; else 4;", STMT,
				"If(test=(1) Then=If(test=(2) Then=(3) Else=(4) un=false isExp=false) Else=null un=false isExp=false)");
		parsesAs("002c-", "if (1) unless (2) 3; else 4;", STMT,
				"If(test=(1) Then=If(test=(2) Then=(3) Else=(4) un=true isExp=false) Else=null un=false isExp=false)");

		taut("002=", "if (1){a;} else {b;}", STMT);
		parsesAs("002d", "if (1) 2; else 3;", STMT, IF(k1, k2, k3));
		parsesAs("002e", "if (1) {2;} else {3;}", STMT, IF(k1, seq(k2), seq(k3)));
		parsesAs("002f", "if (1) {2;}if else {3;}if", STMT, IF(k1, seq(k2), seq(k3)));
		doesntParse("002z", "if (x = 1) {2;}", STMT, "Encountered");
		parses("002h", "unless(1){2;}unless", STMT);
	}

	public void test003_var() throws Exception {
		parsesAs("003a", "x", EXP, "<x>");
		parsesAs("003b", "xborf17", EXP, "<xborf17>");
		parsesAs("003c", "if (x) y;", STMT, "If(test=<x> Then=<y> Else=null un=false isExp=false)");
		parsesAs("003b", "épéé", EXP, "<épéé>");
		parsesAs("003c", "ßüx", EXP, "<ßüx>"); //
		parsesAs("003d", "funny?", EXP, "<funny?>");
		parsesAs("003e", "my·face", EXP, "<my·face>");
		taut("003f", "var x:int;", STMT);
		taut("003f", "var x:int & bool;", STMT);
		taut("003f", "var x:int & bool := 1;", STMT);
	}

	public static String seq(String... ss) {
		return "{" + sep(ss, " ") + "}";
	}

	public void test004_seq() throws Exception {
		parsesAs("004a", "{}", STMT, "{}");
		parsesAs("004b", "{1;}", STMT, "{(1)}");
		parsesAs("004c", "{1;x;}", STMT, "{(1) <x>}");
		parsesAs("004c", "{1;x;}", STMT, seq("(1)", "<x>"));
		parsesAs("004d", "{1;x; {2;y;} }", STMT, "{(1) <x> {(2) <y>}}");
		parsesAs("004e", "if (a) {b;}", STMT, IF(av, seq(bv), no));
		parsesAs("004f", "if (a) {b; c;}", STMT, IF(av, seq(bv, cv), no));
		parsesAs("004g", "if (a) {b; c;} else {1;2;3;}", STMT, IF(av, seq(bv, cv), seq(k1, k2, k3)));
		parsesAs("004h", "if (a) {if (b) c; } else d;", STMT, IF(av, seq(IF(bv, cv, no)), dv));
		parsesAs("004i", "if (a) {if (b) c; } else {d;}", STMT, IF(av, seq(IF(bv, cv, no)), seq(dv)));
		taut("004=", "{}", STMT);
		taut("004=", "{a; b;}", STMT);
		doesntParse("004j", "{}yrud", STMT, "Closing bracket must have id equal to null");
		doesntParse("004j", "{1;}yrud", STMT, "Closing bracket must have id equal to null");
	}

	public static String WHILE(String test, String body) {
		return While.detstr(no, test, body, false, false);
	}

	public static String whyle(String label, String test, String body, boolean un, boolean doo) {
		return While.detstr(label, test, body, "" + un, "" + doo);
	}

	public void test005_while() throws Exception {
		// While(test= body= un= do= label=null)
		parsesAs("005a", "while (x) y;", STMT, whyle(no, xv, yv, false, false)); // "While(test=<x> body=<y> un=false do=false label=null)");
		parsesAs("005b", "until (x) y;", STMT, whyle(no, xv, yv, true, false)); // "While(test=<x> body=<y> un=true do=false label=null)");
		parsesAs("005c", "do y; while(x)", STMT, whyle(no, xv, yv, false, true)); // "While(test=<x> body=<y> un=false do=true label=null)");
		parsesAs("005d", "do y; until(x)", STMT, whyle(no, xv, yv, true, true)); // "While(test=<x> body=<y> un=true do=true label=null)");
		parsesAs("005aL", "l:while (x) y;", STMT, whyle(l, xv, yv, false, false)); // "While(test=<x> body=<y> un=false do=false label=L)");
		parsesAs("005aL", "l:while (x) {y;}while", STMT, whyle(l, xv, seq(yv), false, false)); // "While(test=<x> body=<y> un=false do=false label=L)");
		parsesAs("005aL", "l:while (x) {y;}l", STMT, whyle(l, xv, seq(yv), false, false)); // "While(test=<x> body=<y> un=false do=false label=L)");
		parsesAs("005bL", "l:until (x) y;", STMT, whyle(l, xv, yv, true, false)); //"While(test=<x> body=<y> un=true do=false label=L)");
		parsesAs("005cL", "l:do y; while(x)", STMT, whyle(l, xv, yv, false, true)); //"While(test=<x> body=<y> un=false do=true label=L)");
		parsesAs("005dL", "l:do y; until(x)", STMT, whyle(l, xv, yv, true, true)); //"While(test=<x> body=<y> un=true do=true label=L)");
		parsesAs("005e", "while (x) {y;z;}", STMT, whyle(no, xv, seq(yv, zv), false, false));//"While(test=<x> body={<y> <z>} un=false do=false label=null)");
		parsesAs("005f", "do { y; z;}  while(x)", STMT, whyle(no, xv, seq(yv, zv), false, true)); //"While(test=<x> body={<y> <z>} un=false do=true label=null)");
		parsesAs("005g", "while (x) if (y) z;", STMT, whyle(no, xv, IF(yv, zv, no), false, false));
		parsesAs("005g", "while (x) {if (y) z; else 1; 2;}", STMT, whyle(no, xv, seq(IF(yv, zv, k1), k2), false, false));
		parsesAs("005h", "while (1) until (2) 3;", STMT, whyle(no, k1, whyle(no, k2, k3, true, false), false, false));
		taut("005=", "while(1){fail;}", STMT);
		parsesAs("005i", "while (1) {b;c;}while", STMT, WHILE(k1, seq(bv, cv)));
		parsesAs("005i", "{while (1) {b;c;}while while(2){d;e;}}", STMT, seq(WHILE(k1, seq(bv, cv)), WHILE(k2, seq(dv,
				ev))));
		// If(test= Then= Else= un=false)
	}

	public static String assign(String l, String r) {
		return "Assign(lhs=[" + l + "] rhs=[" + r + "])";
	}

	public static String NOTASS = "Can't convert this expression to an assignment target:";

	public static String assid(String id) {
		return "AssignToId(id=" + id + ")";
	}

	public static String assfr(String target, String field) {
		return "AssignTofield(target=" + target + " field=" + field + ")";
	}

	public void test006_assign() throws Exception {
		// Assign(lhs=[] rhs=[])
		//		parsesAs("006a", "1 := 2", EXP, "Assign(lhs=[(1)] rhs=[(2)])");
		doesntParse("006a", "1 := 2", EXP, NOTASS);
		parsesAs("006av", "x := y", EXP, "Assign(lhs=[" + assid(x) + "] rhs=[<y>])");
		doesntParse("006b", "1,x := 2,y", EXP, NOTASS);

		String ax_gets_by = assign(comma(assid(a), assid(x)), comma(bv, yv));
		parsesAs("006c", "a,x := b,y", EXP, ax_gets_by);
		String agetsb = assign(assid(a), bv);

		taut("006=", "a := a", EXP);
	}

	public static String bind(String pat, String exp) {
		return "Bind(pat=" + pat + " exp=" + exp + ")";
	}

	public void test007_bind_a_var() throws Exception {
		doesntParse("007-basic-a", "@#$@#%", EXP, "Encountered"); // Just checking the doesntParse system a bit.
		// Bind(pat= exp=)
		parsesAs("007a", "x = 1", EXP, "Bind(pat=<~x~> exp=(1))");
		parsesAs("007a2", "x = 1;", STMT, "Bind(pat=<~x~> exp=(1))");
		parsesAs("007c", "{x=1;y=x;}", STMT, "{Bind(pat=<~x~> exp=(1)) Bind(pat=<~y~> exp=<x>)}");
		parsesAs("007d", "1=x", EXP, "Bind(pat=$(1) exp=<x>)");
		parsesAs("007d", "1=x", EXP, bind("$(1)", "<x>"));
		taut("007", "a = a", EXP);
	}

	public static String bin(String x, Object op, String y) {
		return parens(x + op + y);
	}

	public static String una(String op, String x) {
		return parens(op + x);
	}

	public void test008_arith() throws Exception {
		parsesAs("008a", "1+2", EXP, "((1)+(2))");
		parsesAs("008b", "1+2-x", EXP, "(((1)+(2))-<x>)");
		parsesAs("008c", "1+2-x-y", EXP, "((((1)+(2))-<x>)-<y>)");
		parsesAs("008d", "1+2*a-x-y", EXP, "((((1)+((2)*<a>))-<x>)-<y>)");
		parsesAs("008d", "1+2*a*b-x-y", EXP, "((((1)+(((2)*<a>)*<b>))-<x>)-<y>)");
		parsesAs("008d", "1+2*a*b-x*3-y", EXP, "((((1)+(((2)*<a>)*<b>))-(<x>*(3)))-<y>)");
		parsesAs("008a;", "1+2;", STMT, "((1)+(2))");
		parsesAs("008b;", "1+2-x;", STMT, "(((1)+(2))-<x>)");
		parsesAs("008c;", "1+2-x-y;", STMT, "((((1)+(2))-<x>)-<y>)");
		parsesAs("008d;", "1+2*a-x-y;", STMT, "((((1)+((2)*<a>))-<x>)-<y>)");
		parsesAs("008d;", "1+2*a*b-x-y;", STMT, "((((1)+(((2)*<a>)*<b>))-<x>)-<y>)");
		parsesAs("008d;", "1+2*a*b-x*3-y;", STMT, "((((1)+(((2)*<a>)*<b>))-(<x>*(3)))-<y>)");
		parsesAs("008a;:", "1+2;", STMT, "((1)+(2))");
		parsesAs("008b;:", "1+2-x;", STMT, "(((1)+(2))-<x>)");
		parsesAs("008c;:", "1+2-x-y;", STMT, "((((1)+(2))-<x>)-<y>)");
		parsesAs("008d;:", "1+2*a-x-y;", STMT, "((((1)+((2)*<a>))-<x>)-<y>)");
		parsesAs("008d;:", "1+2*a*b-x-y;", STMT, "((((1)+(((2)*<a>)*<b>))-<x>)-<y>)");
		parsesAs("008d;:", "1+2*a*b-x*3-y;", STMT, "((((1)+(((2)*<a>)*<b>))-(<x>*(3)))-<y>)");

		parsesAs("008a;:", "x := 1+2;", STMT, "Assign(lhs=[" + assid(x) + "] rhs=[((1)+(2))])");
		parsesAs("008b;:", "x := 1+2-x;", STMT, "Assign(lhs=[" + assid(x) + "] rhs=[(((1)+(2))-<x>)])");
		parsesAs("008c;:", "x := 1+2-x-y;", STMT, "Assign(lhs=[" + assid(x) + "] rhs=[((((1)+(2))-<x>)-<y>)])");
		parsesAs("008d;:", "x := 1+2*a-x-y;", STMT, "Assign(lhs=[" + assid(x) + "] rhs=[((((1)+((2)*<a>))-<x>)-<y>)])");
		parsesAs("008d;:", "x := 1+2*a*b-x-y;", STMT, "Assign(lhs=[" + assid(x)
				+ "] rhs=[((((1)+(((2)*<a>)*<b>))-<x>)-<y>)])");
		parsesAs("008d;:", "x := 1+2*a*b-x*3-y;", STMT, "Assign(lhs=[" + assid(x)
				+ "] rhs=[((((1)+(((2)*<a>)*<b>))-(<x>*(3)))-<y>)])");

		parsesAs("008a;=", "x = 1+2;", STMT, "Bind(pat=<~x~> exp=((1)+(2)))");
		parsesAs("008b;=", "x = 1+2-x;", STMT, "Bind(pat=<~x~> exp=(((1)+(2))-<x>))");
		parsesAs("008c;=", "x = 1+2-x-y;", STMT, "Bind(pat=<~x~> exp=((((1)+(2))-<x>)-<y>))");
		parsesAs("008d;=", "x = 1+2*a-x-y;", STMT, "Bind(pat=<~x~> exp=((((1)+((2)*<a>))-<x>)-<y>))");
		parsesAs("008d;=", "x = 1+2*a*b-x-y;", STMT, "Bind(pat=<~x~> exp=((((1)+(((2)*<a>)*<b>))-<x>)-<y>))");
		parsesAs("008d;=", "x = 1+2*a*b-x*3-y;", STMT, "Bind(pat=<~x~> exp=((((1)+(((2)*<a>)*<b>))-(<x>*(3)))-<y>))");

		doesntParse("008e", "1+2=a", EXP, NOTPAT);
		taut("008", "1+2", EXP);
		taut("008=", "!1", EXP);
	}

	public void test009_parens() throws Exception {
		parsesAs("009a", "(1)", EXP, "((1))");
		parsesAs("009b", "(1+2)", EXP, "(((1)+(2)))");
		parsesAs("009c", "(1+2)*x", EXP, "((((1)+(2)))*<x>)");
		parsesAs("009d", "y+(1+2)*x", EXP, "(<y>+((((1)+(2)))*<x>))");

		// Pats, unlike Cmds, don't have a Parens-like construct per se.  No need, I thought. Thus:
		parsesAs("009f", "(x)=1", EXP, "Bind(pat=<~x~> exp=(1))");
		parsesAs("009g", "(x)=(1)", EXP, "Bind(pat=<~x~> exp=((1)))");
		parsesAs("009f;", "(x)=1;", STMT, "Bind(pat=<~x~> exp=(1))");
		parsesAs("009g;", "(x)=(1);", STMT, "Bind(pat=<~x~> exp=((1)))");
		taut("009=", "(1)", EXP);

	}

	public static String fr(String target, String fld) {
		return "FieldRef(target=" + target + " field=" + fld + ")";
	}

	public static String fc(String functi, String arg) {
		return "FunCall(function=" + functi + " args=[" + arg + "])";
	}

	public static String mc(String tar, String met, String args) {
		return "MethodCall(target=" + tar + " method=" + met + " args=[" + args + "])";
	}

	public static final String a = "a";
	public static final String av = "<a>";
	public static final String ap = "<~a~>";

	public static final String b = "b";
	public static final String bv = "<b>";
	public static final String bp = "<~b~>";

	public static final String c = "c";
	public static final String cv = "<c>";
	public static final String cp = "<~c~>";

	public static final String d = "d";
	public static final String dv = "<d>";
	public static final String dp = "<~d~>";

	public static final String e = "e";
	public static final String ev = "<e>";
	public static final String ep = "<~e~>";

	public static final String f = "f";
	public static final String fv = "<f>";
	public static final String fp = "<~f~>";

	public static final String g = "g";
	public static final String gv = "<g>";
	public static final String gp = "<~g~>";

	public static final String h = "h";
	public static final String hv = "<h>";
	public static final String hp = "<~h~>";

	public static final String i = "i";
	public static final String iv = "<i>";
	public static final String ip = "<~i~>";

	public static final String j = "j";
	public static final String jv = "<j>";
	public static final String jp = "<~j~>";

	public static final String k = "k";
	public static final String kv = "<k>";
	public static final String kp = "<~k~>";

	public static final String l = "l";
	public static final String lv = "<l>";
	public static final String lp = "<~l~>";

	public static final String m = "m";
	public static final String mv = "<m>";
	public static final String mp = "<~m~>";

	public static final String n = "n";
	public static final String nv = "<n>";
	public static final String np = "<~n~>";

	public static final String o = "o";
	public static final String ov = "<o>";
	public static final String op = "<~o~>";

	public static final String p = "p";
	public static final String pv = "<p>";
	public static final String pp = "<~p~>";

	public static final String q = "q";
	public static final String qv = "<q>";
	public static final String qp = "<~q~>";

	public static final String r = "r";
	public static final String rv = "<r>";
	public static final String rp = "<~r~>";

	public static final String s = "s";
	public static final String sv = "<s>";
	public static final String sp = "<~s~>";

	public static final String t = "t";
	public static final String tv = "<t>";
	public static final String tp = "<~t~>";

	public static final String u = "u";
	public static final String uv = "<u>";
	public static final String up = "<~u~>";

	public static final String v = "v";
	public static final String vv = "<v>";
	public static final String vp = "<~v~>";

	public static final String w = "w";
	public static final String wv = "<w>";
	public static final String wp = "<~w~>";

	public static final String x = "x";
	public static final String xv = "<x>";
	public static final String xp = "<~x~>";

	public static final String y = "y";
	public static final String yv = "<y>";
	public static final String yp = "<~y~>";

	public static final String z = "z";
	public static final String zv = "<z>";
	public static final String zp = "<~z~>";

	public static final String k0 = "(0)";
	public static final String k1 = "(1)";
	public static final String k2 = "(2)";
	public static final String k3 = "(3)";
	public static final String k4 = "(4)";

	public void test010_select_funcall_methodcall() throws Exception {
		// FieldRef(target= field=)
		// MethodCall(target= method= args=[])
		// FunCall(function= args=[])
		parsesAs("010a", "1.a", EXP, "FieldRef(target=(1) field=a)");
		parsesAs("010a", "1.a", EXP, fr(k1, a));

		parsesAs("010b", "1(a)", EXP, "FunCall(function=(1) args=[<a>])");
		parsesAs("010c", "1.b(a)", EXP, "MethodCall(target=(1) method=b args=[<a>])");

		parsesAs("010d", "1.a.b", EXP, "FieldRef(target=FieldRef(target=(1) field=a) field=b)");
		parsesAs("010f", "1.a()", EXP, "MethodCall(target=(1) method=a args=[])");
		parsesAs("010e", "1.a(2,b)", EXP, "MethodCall(target=(1) method=a args=[(2),<b>])");
		parsesAs("010h", "1(2)()", EXP, "FunCall(function=FunCall(function=(1) args=[(2)]) args=[])");
		parsesAs("010i", "1.f()()", EXP, "FunCall(function=MethodCall(target=(1) method=f args=[]) args=[])");
		parsesAs("010j", "1().m", EXP, "FieldRef(target=FunCall(function=(1) args=[]) field=m)");
		parsesAs("010k", "1().m()", EXP, "MethodCall(target=FunCall(function=(1) args=[]) method=m args=[])");
		parsesAs("010l", "1.a.b()", EXP, "MethodCall(target=FieldRef(target=(1) field=a) method=b args=[])");
		parsesAs("010m1", "a.m().b", EXP, "FieldRef(target=MethodCall(target=<a> method=m args=[]) field=b)");
		parsesAs("010m2", "a.m(2,3).b", EXP, "FieldRef(target=MethodCall(target=<a> method=m args=[(2),(3)]) field=b)");
		parsesAs("010n1", "a.b.c().d", EXP,
				"FieldRef(target=MethodCall(target=FieldRef(target=<a> field=b) method=c args=[]) field=d)");
		parsesAs("010n2", "a.b.c().d()", EXP,
				"MethodCall(target=MethodCall(target=FieldRef(target=<a> field=b) method=c args=[]) method=d args=[])");
		// More sequences

		// Fitting it into the parse order
		parsesAs("010g", "1+2(3)", EXP, "((1)+FunCall(function=(2) args=[(3)]))");
		parsesAs("010o1", "a=b(c)", EXP, "Bind(pat=<~a~> exp=FunCall(function=<b> args=[<c>]))");
		parsesAs("010o2", "a=b.x(c)", EXP, "Bind(pat=<~a~> exp=MethodCall(target=<b> method=x args=[<c>]))");
		parsesAs("010o3", "a=x(c)", EXP, "Bind(pat=<~a~> exp=FunCall(function=<x> args=[<c>]))");
		parsesAs("010p1", "a.b := b.a", EXP, "Assign(lhs=[" + assfr(av, b) + "] rhs=[FieldRef(target=<b> field=a)])");
		parsesAs("010p2", "a.b, c := b.a", EXP, "Assign(lhs=[" + comma(assfr(av, b), assid(c))
				+ "] rhs=[FieldRef(target=<b> field=a)])");

		parsesAs("010q1", "f(g(1))", EXP, "FunCall(function=<f> args=[FunCall(function=<g> args=[(1)])])");
		parsesAs("010q1", "f(g(1,2))", EXP, "FunCall(function=<f> args=[FunCall(function=<g> args=[(1),(2)])])");
		parsesAs("010q2", "f(g())", EXP, "FunCall(function=<f> args=[FunCall(function=<g> args=[])])");
		parsesAs("010q2", "f(g.g())", EXP, "FunCall(function=<f> args=[MethodCall(target=<g> method=g args=[])])");

		taut("010=", "f(1)", EXP);
		taut("010=", "f.g", EXP);
		taut("010=", "f.g(1)", EXP);
		parses("010r1", "f(,)", EXP);
		parses("010r2", "f(x,)", EXP);
		parses("010r3", "f(x,y,)", EXP);
		parses("010r4", "f(x,f(y,),)", EXP);

	}

	public static String anonfun(String formals, String body) {
		return anonfun(formals, body, false);
	}

	public static String anonfun(String formals, String body, boolean purity) {
		return AnonFun.detstr(funbody(purity, monobody(formals, body, purity)), purity);
		//return "AnonFun(fun=" + funbody(purity, monobody(formals, body, purity)) + ")";
	}

	// usage: monobody(formals(),)
	public static String monobody(String formals, String body) {
		return monobody(formals, body, false);
	}

	public static String monobody(String formals, String body, boolean purity) {
		return MonoBody.detstr(no, formals, null, null, false, 0, body, false, purity);
	}

	public static String monobody(String id, String formals, String body) {
		return monobody(id, formals, body, false);
	}

	public static String monobody2(String id, String formals, String body, boolean checked, boolean purity) {
		return MonoBody.detstr(id, formals, null, null, false, 0, body, checked, purity);
	}
	public static String monobody(String id, String formals, String body, boolean purity) {
		return MonoBody.detstr(id, formals, null, null, false, 0, body, false, purity);
	}

	public static String funbody(String... monobodies) {
		return funbody(false, monobodies);
	}

	public static String funbody(boolean purity, String... monobodies) {
		return FunBody.detstr(dearr(monobodies), purity);
	}

	public static String formals(String... patformals) {
		return parens(patformals);
	}

	public static String parens(String... x) {
		return "(" + sep(x, ",") + ")";
	}

	public static String delist(List<? extends Object> stuff) {
		return "[" + sep(stuff, ",") + "]";
	}

	public static String anonfun(String funbody) {
		return AnonFun.detstr(funbody, false);
	}

	public static String anonfun1(String patformals, String body) {
		return anonfun(funbody(monobody(formals(patformals), body)));
	}

	public static String comma(String... strings) {
		return sep(strings, ",");
	}

	public void test011_fn() throws Exception {

		doesntParse("011a", "fn = x = 1", EXP, NOTPAT);

		parsesAs("011=b", "fn x = 1", EXP, anonfun(funbody(monobody(formals(xp), k1))));
		parsesAs("011=b", "fn x = 1", EXP, anonfun1(xp, k1));

		parsesAs("011=b", "fn (x) = 1", EXP, anonfun1(xp, k1));
		parsesAs("011=b", "fn (x,y) = 1", EXP, anonfun1(comma(xp, yp), k1));
		parsesAs("011=b", "fn () = 1", EXP, anonfun1(comma(), k1));
		parsesAs("011=B", "fn (x) = 1", EXP, anonfun1(comma(xp), k1));

		parsesAs("011=c", "fn x = x+y", EXP, anonfun1(comma(xp), bin(xv, PLUS, yv)));

		parsesAs("011=c", "fn (x) = x+y", EXP, anonfun1(comma(xp), bin(xv, PLUS, yv)));
		parsesAs("011=c", "fn (x,y) = x+y", EXP, anonfun1(comma(xp, yp), bin(xv, PLUS, yv)));
		parsesAs("011=c", "fn () = x+y", EXP, anonfun1(comma(), bin(xv, PLUS, yv)));

		parsesAs("011=d", "fn = (fn = 1)", EXP, anonfun1(comma(), parens(anonfun1(comma(), k1))));
		doesntParse("011=-d", "fn = fn = 1", EXP, "Encountered \"fn\"");

		parsesAs("011=e", "f = fn x = 1+x", EXP, bind("<~f~>", anonfun1(comma(xp), bin(k1, PLUS, xv))));
		parsesAs("011=e", "f:= fn x = 1+x", EXP, assign(assid(f), anonfun1(comma(xp), bin(k1, PLUS, xv))));
		parsesAs("011=f", "fn = {1;2;}", EXP, anonfun1(comma(), seq(k1, k2)));
		parsesAs("011=f", "fn = {1;2;}", EXP, anonfun1(comma(), seq(k1, k2)));

		//test multiple clauses
		parsesAs("011=g", "fn (_) = 1 | x = 2", EXP, anonfun(funbody(false, monobody(formals(patwild()), k1), monobody(
				formals(xp), k2))));

		parsesAs("011=h", "f(fn (x,y) = x+1 | (x,z) = x.z)", EXP, fc(fv, anonfun(funbody(monobody(formals(xp, yp), bin(
				xv, PLUS, k1)), monobody(formals(xp, zp), fr(xv, z))))));

		// Block in fun body

		parsesAs("011j", "fn (x) {x;}", EXP, anonfun(formals(xp), seq(xv)));
		parsesAs("011j2", "fn (x) = {x;}", EXP, anonfun(formals(xp), seq(xv)));

		parsesAs("011k", "fn (x) {x; x:=1;}", EXP, anonfun(formals(xp), seq(xv, assign(assid(x), k1))));
		parsesAs("011k", "fn (x) = {x; x:=1;}", EXP, anonfun(formals(xp), seq(xv, assign(assid(x), k1))));
		parses("011L", "fn x = 1", EXP);
		parses("011L", "fn(x) = 1", EXP);
		parses("011L", "fn x {1;}", EXP);
		parses("011L", "fn(x){1;}", EXP);
		parses("011L", "fn x={1;}", EXP);
		parses("011L", "fn(x)={1;}", EXP);
		//		parses("011L", "fn x => 1", EXP);
		//		parses("011L", "fn(x) => 1", EXP);
		//		parses("011L", "fn x=>{1;}", EXP);
		//		parses("011L", "fn(x)=>{1;}", EXP);

		parses("011m", "fn 1 = 1", EXP);
		parses("011m", "fn 1 = 1 | 2 = 3 | x = x | (2) = 4", EXP);

		taut("011=", "fn (1) = 1", EXP);
		taut("011=", "fn (1) = 1 | (x) = 2", EXP);

		parsesAs("011o", "fn () = {x;}fn", EXP, anonfun(formals(), seq(xv)));

	}

	public static String fundecl(String name, String funbody) {
		return fundecl(name, funbody, false);
	}

	public static String fundecl(String name, String funbody, boolean purity) {
		return FunDecl.detstr(name, funbody, purity);
	}

	public static String fundecl(String name, String args, String ret) {
		return fundecl(name, args, ret, false);
	}

	public static String fundecl(String name, String args, String ret, boolean purity) {
		return fundecl(name, funbody(monobody(name, formals(args), ret, purity)));
	}

	public void test012_fun() throws Exception {
		// Wow! So many syntactic variations!
		parsesAs("012a1", "fun f(x) = 1;", STMT, fundecl(f, xp, k1));
		parsesAs("012a2", "fun f(x) = 1;", STMT, fundecl(f, xp, k1));
		parsesAs("012a3", "fun f(x,y) = 1;", STMT, fundecl(f, comma(xp, yp), k1));
		parsesAs("012a4", "fun f(x) { 1;}", STMT, fundecl(f, xp, seq(k1)));
		parsesAs("012a5", "fun f(x) = { 1;}", STMT, fundecl(f, xp, seq(k1)));
		//		parsesAs("012a", "fun f(x) => { 1;}", STMT, fundecl(f, fun(xp, seq(k1))));
		//		parsesAs("012a", "fun f(x) => 1;", STMT, fundecl(f, fun(xp, k1)));
		parsesAs("012a6", "fun f x = 1;", STMT, fundecl(f, xp, k1));
		parsesAs("012a7", "fun f x { 1;}", STMT, fundecl(f, xp, seq(k1)));
		parsesAs("012a8", "fun f x = { 1;}", STMT, fundecl(f, xp, seq(k1)));
		//		parsesAs("012a", "fun f x => { 1;}", STMT, fundecl(f, fun(xp, seq(k1))));
		//		parsesAs("012a", "fun f x => 1;", STMT, fundecl(f, fun(xp, k1)));

		parsesAs("012b1", "fun f (x) = 1; |  f (y) = 2;", STMT, fundecl(f, funbody(monobody(f, formals(xp), k1),
				monobody(f, formals(yp), k2))));

		parsesAs("012b2", "fun f (x) { 1; } | f (y) { 2; }", STMT, fundecl(f, funbody(
				monobody(f, formals(xp), seq(k1)), monobody(f, formals(yp), seq(k2)))));

		String fyy = anonfun(formals(yp), yv);
		parsesAs("012c1", "fun f x = (fn y = y);", STMT, fundecl(f, xp, parens(fyy)));
		parsesAs("012c2", "fun f x { fn y = y; }", STMT, fundecl(f, xp, seq(fyy)));
		String bodd = seq(k1, k2, bind(xp, yv), assign(assid(x), yv));
		parsesAs("012d", "fun f() = {1;2;x=y;x:=y;}", STMT, fundecl(f, "", bodd));
		parsesAs("012d", "fun f()   {1;2;x=y;x:=y;}", STMT, fundecl(f, "", bodd));

		parsesAs("012e", "fun f(x,y) = x+y;", STMT, fundecl(f, comma(xp, yp), bin(xv, PLUS, yv)));
		parsesAs("012f", "fun s [x, y...] = x + s(y); | s [] = 1;", STMT, fundecl(s, funbody(monobody(s,
				formals(patlistctor(patlb(xp), patlbe(yp))), bin(xv, PLUS, fc(sv, yv))), monobody(s,
				formals(patlistctor()), k1))));
		parsesAs("012f2", "fun s ([x, y...]) = x + s(y); | s ([]) = 1;", STMT, fundecl(s, funbody(monobody(s,
				formals(patlistctor(patlb(xp), patlbe(yp))), bin(xv, PLUS, fc(sv, yv))), monobody(s,
				formals(patlistctor()), k1))));
		parsesAs("012f3", "fun s ([x, y...], b) = x + s(y); | s ([], b) = b;", STMT, fundecl(s, funbody(monobody(s,
				formals(patlistctor(patlb(xp), patlbe(yp)), bp), bin(xv, PLUS, fc(sv, yv))), monobody(s, formals(
				patlistctor(), bp), bv))));

		parsesAs("012f4", "fun s ([x, y...], b) = {b(x); s(y,b);} | s ([], b) = b;", STMT, fundecl(s, funbody(monobody(
				s, formals(patlistctor(patlb(xp), patlbe(yp)), bp), seq(fc(bv, xv), fc(sv, comma(yv, bv)))), monobody(
				s, formals(patlistctor(), bp), bv))));
		parsesAs("012f5", "fun s ([x, y...], b)   {b(x); s(y,b);} | s ([], b) = b;", STMT, fundecl(s, funbody(monobody(
				s, formals(patlistctor(patlb(xp), patlbe(yp)), bp), seq(fc(bv, xv), fc(sv, comma(yv, bv)))), monobody(
				s, formals(patlistctor(), bp), bv))));
		taut("012=", "fun x(y) = 1;", STMT);
		taut("012=", "fun f(x) = 1; | f(g) = 2;", STMT);
		taut("012=", "fun f(1) = 1; | f(2) = 2;", STMT);
		
		parsesAs("012g", "fun s (x) = {x;}fun", STMT, fundecl(s, xp, seq(xv)));
		parsesAs("012g", "fun s (x) = {x;}s", STMT, fundecl(s, xp, seq(xv)));
		doesntParse("012g", "fun s (x) = {x;}slut", STMT, "Closing bracket must have id equal to fun or s");

		parses("012h1,", "fun f(x,) = x;", STMT);
		parses("012h0,", "fun f(,) = x;", STMT);
		parses("012h2,", "fun f(x,y,) = x;", STMT);
		parses("012h2q,", "fun f(x, 'ow'/[] ,) = x;", STMT);
		parses("012h2r,", "fun f(x, [a,b,c,] ,) = x;", STMT);
	}

	public static String listctor(String... bits) {
		return "ListCtor(bits=[" + sep(bits, ",") + "])";
	}

	public static String lb(String exp) {
		return "ListBitExp(exp=" + exp + ")";
	}

	public static String lbe(String exp) {
		return "ListBitEllip(exp=" + exp + ")";
	}

	public void test013_list_ctor() throws Exception {
		parsesAs("013a", "[]", EXP, listctor());
		parsesAs("013a", "[a]", EXP, listctor(lb(av)));
		parsesAs("013a,", "[a,]", EXP, listctor(lb(av)));
		parsesAs("013a", "[a,b]", EXP, listctor(lb(av), lb(bv)));
		parsesAs("013ab,", "[a,b,]", EXP, listctor(lb(av), lb(bv)));
		parsesAs("013a", "[a...,b]", EXP, listctor(lbe(av), lb(bv)));
		parsesAs("013a3,", "[a...,b,]", EXP, listctor(lbe(av), lb(bv)));
		parsesAs("013a", "[a...,b...]", EXP, listctor(lbe(av), lbe(bv)));
		parsesAs("013a4,", "[a...,b...,]", EXP, listctor(lbe(av), lbe(bv)));
		parsesAs("013b", "[[]]", EXP, listctor(lb(listctor())));
		parsesAs("013b", "[[]...]", EXP, listctor(lbe(listctor())));
		parsesAs("013b", "[[]...,[]]", EXP, listctor(lbe(listctor()), lb(listctor())));
		taut("013=", "[1, 2, 3]", EXP);
	}

	public static String casprio(String pat, String prio, String body) {
		return Case.detstr(pat, null, null, true, prio, false, body);
	}

	
	
	public static String caschecked(String pat,  String body) {
		return Case.detstr(pat, null, null, false, 0,true, body);
	}
	public static String cas(String pat, String prio, String body) {
		return Case.detstr(pat, null, null, false, prio,false, body);
	}

	public static String cas(String pat, String body) {
		return cas(pat, "0", body);
	}

	public static String match(String subject, String... cases) {
		return "Match(subject=" + subject + " cases=[" + sep(cases, ",") + "])";
	}

	public static final String no = "null";
	public static final List<String> NO = new ArrayList<String>(0);
	public static final String none = "";

	public void test014_match() throws Exception {
		parsesAs("014a", "match(1){a = 1}", STMT, match(parens(k1), cas(ap, k1)));
		parsesAs("014a", "match(1){a = 1 | b = 2}", STMT, match(parens(k1), cas(ap, k1), cas(bp, k2)));
		parsesAs("014a", "match(1){a = 1 | b = {2;3;}}", STMT, match(parens(k1), cas(ap, k1), cas(bp, seq(k2, k3))));
		parsesAs("014a", "match(1){| a = 1 | b = {2;3;}}", STMT, match(parens(k1), cas(ap, k1), cas(bp, seq(k2, k3))));

		parsesAs("014b", "match 1+2 {a = 1}", STMT, match(bin(k1, PLUS, k2), cas(ap, k1)));
		parsesAs("014b", "match 1+2 {a = 1}match", STMT, match(bin(k1, PLUS, k2), cas(ap, k1)));
		parsesAs("014b", "match 1.f {a = 1}", STMT, match(fr(k1, f), cas(ap, k1)));
		parsesAs("014b", "match 1.f {a prio 4 = 1}", STMT, match(fr(k1, f), casprio(ap, "4", k1)));

		String cod = "{x:=y; z = f(x);}";
		String bod = seq(assign(assid(x), yv), bind(zp, fc(fv, xv)));
		parsesAs("014c", cod, STMT, bod);
		parsesAs("014c", "match f(x,y,z) { a = " + cod + "}", STMT, match(fc(fv, comma(xv, yv, zv)), cas(ap, bod)));

		parsesAs("014c", "match f(x,y,z) { a = " + cod + "}match", STMT, match(fc(fv, comma(xv, yv, zv)), cas(ap, bod)));

		taut("014=", "match (1){x = {1; 2;}}", STMT);

	}

	public static String trye(String body, List<String> cases, String fin) {
		return "Try(body=" + body + " cases=[" + sep(cases, ",") + "] fin=" + fin + ")";
	}

	public void test015_try() throws Exception {
		doesntParse("015a", "try {x := y;}", STMT, "a 'try' must have a 'catch' or a 'finally'");
		String ass = assign(assid(x), yv);
		parsesAs("015", "x := y;", STMT, ass);
		String adoom = cas(ap, fc(dv, ""));
		String adooms = cas(ap, seq(fc(dv, "")));
		String doom1 = cas("$(1)", seq(fc(ev, ""), fc(xv, "")));
		parsesAs("015b", "try {x := y;} catch { a = d() }", STMT, trye(seq(ass), list(adoom), no));
		parsesAs("015b", "try {x := y;} catch { a = {d();} }", STMT, trye(seq(ass), list(adooms), no));
		parsesAs("015b", "try {x := y;} catch { a = d() } finally {e(f);}", STMT, trye(seq(ass), list(adoom), seq(fc(
				ev, fv))));
		parsesAs("015b", "try {x := y;} catch { a = d() | 1 = {e();x();}} finally {e(f);}", STMT, trye(seq(ass), list(
				adoom, doom1), seq(fc(ev, fv))));
		parsesAs("015b", "try {x := y;}  finally {e(f);}", STMT, trye(seq(ass), list(""), seq(fc(ev, fv))));
		taut("015=", "try {1;} catch {a = {2;}} finally {x := 3;}", STMT);
		parsesAs("015f", "try {x := y;}try catch { a = d() }catch finally {e(f);}finally", STMT, trye(seq(ass),
				list(adoom), seq(fc(ev, fv))));

	}

	public static String qn(String... bits) {
		return "QualName(ids=[" + sep(bits, ",") + "])";
	}

	public static String supe(String qn, String def, String... args) {
		return SuperCall.detstr(qn, dearr(args), def);
	}

	public void test016_supercall() throws Exception {
		parsesAs("016a", "super.f()", EXP, supe(no, f));
		parsesAs("016a", "super.f().g()", EXP, mc(supe(no, f), g, ""));
		parsesAs("016a", "super.f().g", EXP, fr(supe(no, f), g));
		parsesAs("016a", "super.f()+1", EXP, bin(supe(no, f), PLUS, k1));

		parsesAs("016b", "super.f(1)", EXP, supe(no, f, k1));
		parsesAs("016b", "super.f(1, x)", EXP, supe(no, f, k1, xv));
		parsesAs("016b", "super.f(1, x, super.f())", EXP, supe(no, f, k1, xv, supe(no, f)));
		parsesAs("016b", "super.f(1, x, super.f()+4)", EXP, supe(no, f, k1, xv, bin(supe(no, f), PLUS, k4)));

		doesntParse("016c", "super@a()", EXP, "qualified name (a) must have at least 2 elements, but it has only 1");
		parsesAs("016c", "super@a.b()", EXP, supe(qn(a), b));
		parsesAs("016c", "super@a.a.b()", EXP, supe(qn(a, a), b));
		parsesAs("016c", "super@a.a.x.b()", EXP, supe(qn(a, a, x), b));

		parsesAs("016d", "super@a.b(s)", EXP, supe(qn(a), b, sv));
		parsesAs("016d", "1+super@a.b(s)", EXP, bin(k1, PLUS, supe(qn(a), b, sv)));
		parsesAs("016d", "1+super@a.b(s)*2", EXP, bin(k1, PLUS, bin(supe(qn(a), b, sv), TIMES, k2)));

		taut("016=", "super.f()", EXP);
		taut("016=", "super@A.f()", EXP);
		parses("016e", "super.f(,)", EXP);
		parses("016e", "super.f(1,)", EXP);
		parses("016e", "super.f(1,2,)", EXP);
	}

	public static String rf(String id, String exp) {
		return "RecordField(id=" + id + " exp=" + exp + ")";
	}

	public static String rec(String... fields) {
		return "RecordCtor(fields=[" + sep(fields, ",") + "])";
	}

	public void test017_record_constructor() throws Exception {
		parsesAs("017a", "{::}", EXP, rec());
		parsesAs("017a2", "‹›", EXP, rec());
		parsesAs("017va", "<>", EXP, rec());
		parsesAs("017a3", "{: a:x:}", EXP, rec(rf(a, xv)));
		parsesAs("017a3v", "< a=x >", EXP, rec(rf(a, xv)));
		parsesAs("017a4", "< a= fn(x)=1>", EXP, rec(rf(a, anonfun(formals(xp), k1))));
		parsesAs("017a4", "{: a: fn(x)=1:}", EXP, rec(rf(a, anonfun(formals(xp), k1))));
		parsesAs("017a5", "{: a: fn(x)=1, b: fn(x)=1|(y)=2:}", EXP, rec(rf(a, anonfun(formals(xp), k1)), rf(b,
				anonfun(funbody(monobody(formals(xp), k1), monobody(formals(yp), k2))))));
		parsesAs("017a5", "< a= fn(x)=1, b= fn(x)=1|(y)=2 >", EXP, rec(rf(a, anonfun(formals(xp), k1)), rf(b,
				anonfun(funbody(monobody(formals(xp), k1), monobody(formals(yp), k2))))));

		parsesAs("017b", "x = {:a:x:}", EXP, bind(xp, rec(rf(a, xv))));
		parsesAs("017b", "x = {::}", EXP, bind(xp, rec()));
		parsesAs("017b", "x := {:a:x:}", EXP, assign(assid(x), rec(rf(a, xv))));
		parsesAs("017b", "x := {:a:x, c:d:}", EXP, assign(assid(x), rec(rf(a, xv), rf(c, dv))));

		parsesAs("017c", "{: a:1, b:2, c:3, d:4 :}", EXP, rec(rf(a, k1), rf(b, k2), rf(c, k3), rf(d, k4)));
		parsesAs("017c", "{: a:1, b:2+x, c:3, d:4 :}", EXP, rec(rf(a, k1), rf(b, bin(k2, PLUS, xv)), rf(c, k3), rf(d,
				k4)));

		taut("017=", "‹a:1, b:2›", EXP);
	}

	public void test018_mult_div() throws Exception {
		parsesAs("018a", "1/2", EXP, bin(k1, FDIV, k2));
		parsesAs("018a", "1/2 + 3", EXP, bin(bin(k1, FDIV, k2), PLUS, k3));
		parsesAs("018a", "1 div 2 + 3", EXP, bin(bin(k1, IDIV, k2), PLUS, k3));
		parsesAs("018a", "1 ÷ 2 + 3", EXP, bin(bin(k1, IDIV, k2), PLUS, k3));
		parsesAs("018a", "1 div 2", EXP, bin(k1, IDIV, k2));

		parsesAs("018b", "a*b/c", EXP, bin(bin(av, TIMES, bv), FDIV, cv));
		doesntParse("018b", "a*b/c*d", EXP, "Encountered \"*\"");
		doesntParse("018b", "a*b div c*d", EXP, "Encountered \"*\"");
		doesntParse("018b", "a*b/c/d", EXP, "Encountered \"/\"");
		doesntParse("018b", "a*b div c/d", EXP, "Encountered \"/\"");
		doesntParse("018b", "a*b / c div d", EXP, "Encountered \"div\"");
		doesntParse("018b", "a*b div c div d", EXP, "Encountered \"div\"");

		parsesAs("018c", "a*b/(c*d)", EXP, bin(bin(av, TIMES, bv), FDIV, parens(bin(cv, TIMES, dv))));
		parsesAs("018b", "(a*b/c)/d", EXP, bin(parens(bin(bin(av, TIMES, bv), FDIV, cv)), FDIV, dv));
		taut("018=", "1*2", EXP);
		taut("018=", "1*2+3", EXP);
		taut("018=", "1*(2+3)", EXP);
	}

	public static String varop(Op op, String... args) {
		return "(" + sep(args, op.str) + ")";
	}

	public void test019_logical_operations() throws Exception {
		parsesAs("019a", "a && b", EXP, varop(AND, av, bv));
		parsesAs("019a", "a && b && c", EXP, varop(AND, av, bv, cv));
		parsesAs("019a", "a && b && c && d", EXP, varop(AND, av, bv, cv, dv));
		parsesAs("019b", "a = a && b;", STMT, bind(ap, varop(AND, av, bv)));
		parsesAs("019c", "fn(x)=x && 1 && y", EXP, anonfun(formals(xp), varop(AND, xv, k1, yv)));

		parsesAs("019a", "a ^^ b", EXP, varop(ONEOF, av, bv));
		parsesAs("019a", "a ^^ b ^^ c", EXP, varop(ONEOF, av, bv, cv));
		parsesAs("019a", "a ^^ b ^^ c ^^ d", EXP, varop(ONEOF, av, bv, cv, dv));
		parsesAs("019b", "a = a ^^ b;", STMT, bind(ap, varop(ONEOF, av, bv)));
		parsesAs("019c", "fn(x)=x ^^ 1 ^^ y", EXP, anonfun(formals(xp), varop(ONEOF, xv, k1, yv))); // TODO: implement ||, ^^, and check that they parse.

		parsesAs("019a", "a || b", EXP, varop(OR, av, bv));
		parsesAs("019a", "a || b || c", EXP, varop(OR, av, bv, cv));
		parsesAs("019a", "a || b || c || d", EXP, varop(OR, av, bv, cv, dv));
		parsesAs("019b", "a = a || b;", STMT, bind(ap, varop(OR, av, bv)));
		parsesAs("019c", "fn(x)=x || 1 || y", EXP, anonfun(formals(xp), varop(OR, xv, k1, yv))); // TODO: implement ||, ^^, and check that they parse.

		doesntParse("019d", "a && b || c", EXP, "Encountered \"||\"");
		doesntParse("019d", "a && b ^^ c", EXP, "Encountered \"^^\"");
		doesntParse("019d", "a || b && c", EXP, "Encountered \"&&\"");
		doesntParse("019d", "a || b ^^ c", EXP, "Encountered \"^^\"");
		doesntParse("019d", "a ^^ b && c", EXP, "Encountered \"&&\"");
		doesntParse("019d", "a ^^ b || c", EXP, "Encountered \"||\"");

		String bp1 = bin(bv, PLUS, k1);
		parsesAs("019e", "a && b + 1", EXP, varop(AND, av, bp1));
		parsesAs("019e", "a || b + 1", EXP, varop(OR, av, bp1));
		parsesAs("019e", "a ^^ b + 1", EXP, varop(ONEOF, av, bp1));

		taut("019=", "a&&b", EXP);
	}

	// Make sure that there are tests with operations of different precedences on the right of comparisons

	public static String comp(Object... objects) {
		String sb = "Comparison(first=" + objects[0].toString() + " rest=[";
		int i = 1;
		while (i + 1 < objects.length) {
			String cb = "ComparisonBit(comparison=" + objects[i] + " to=" + objects[i + 1] + ")";
			sb += cb;
			if (i + 3 < objects.length)
				sb += ",";
			i += 2;
		}
		sb += "])";
		return sb;
	}

	private void varyComp(String loc, String exp, String des) throws Exception {
		varyComp(loc, exp, EXP, des);
	}

	private void varyComp(String loc, String exp, SyntacticClass scls, String des) throws Exception {
		for (ComparisonOp co : ComparisonOp.values()) {
			String e = exp.replaceAll("@@", co.text);
			String d = des.replaceAll("@@", co.text);
			for (ComparisonOp co2 : ComparisonOp.values()) {
				String e2 = e.replaceAll("€€", co.text);
				String d2 = d.replaceAll("€€", co.text);
				parsesAs(loc + co + co, e2, scls, d2);
			}
		}
	}

	public void test020_comparisons() throws Exception {
		parsesAs("020a", "a<b", EXP, comp(av, LT, bv));
		parsesAs("020a", "a≤b", EXP, comp(av, LE, bv));
		parsesAs("020a", "a≥b", EXP, comp(av, GE, bv));
		parsesAs("020a", "a≠b", EXP, comp(av, NE, bv));
		varyComp("020a", "a @@ b", comp(av, "@@", bv));
		varyComp("020b", "a @@ b @@ c", comp(av, "@@", bv, "@@", cv));
		varyComp("020c", "a @@ b + 1", comp(av, "@@", bin(bv, PLUS, k1)));
		varyComp("020c", "a + 2 @@ b + 1", comp(bin(av, PLUS, k2), "@@", bin(bv, PLUS, k1)));
		varyComp("020c", "a * 2 @@ b div 1", comp(bin(av, TIMES, k2), "@@", bin(bv, IDIV, k1)));
		varyComp("020d", "a = b @@ c", bind(ap, comp(bv, "@@", cv)));
		varyComp("020e", "a := b @@ c €€ d", assign(assid(a), comp(bv, "@@", cv, "€€", dv)));
		varyComp("020e", "a := b + 1 * 2 @@ c * d + e €€ d + 1 / 2", assign(assid(a), comp(bin(bv, PLUS, bin(k1, TIMES,
				k2)), "@@", bin(bin(cv, TIMES, dv), PLUS, ev), "€€", bin(dv, PLUS, bin(k1, FDIV, k2)))));
		varyComp("020f", "if (a @@ b) {a := a €€ c;}", STMT, IF(comp(av, "@@", bv), seq(assign(assid(a), comp(av, "€€",
				cv))), no));
		taut("020-", "a < b", EXP);
		taut("020-", "a < b ≤ c ≠ e ≥ d", EXP);

	}

	public void vary(String loc, String exp, SyntacticClass scls, String des, String var, List<? extends Object> L)
			throws Exception {
		for (Object rep : L) {
			String e = exp.replace(var, rep.toString());
			String d = des.replace(var, rep.toString());
			parsesAs(loc + rep, e, scls, d);
		}
	}

	public void vary2(String loc, String exp, SyntacticClass scls, String des, String var1, List<? extends Object> L1,
			String var2, List<? extends Object> L2) throws Exception {
		for (Object rep : L1) {
			String e = exp.replace(var1, rep.toString());
			String d = des.replace(var1, rep.toString());
			for (Object rep2 : L2) {
				String e2 = e.replace(var2, rep2.toString());
				String d2 = d.replace(var2, rep2.toString());
				parsesAs(loc + rep + rep2, e2, scls, d2);
			}
		}
	}

	public void test021_unary() throws Exception {
		parsesAs("021a0", "- 1", EXP, una("-", k1));
		List<String> unaries = list("-", "!");
		vary("021a", "@ a", EXP, una("@", av), "@", unaries);
		vary2("021b", "@ a X 1", EXP, bin(una("@", av), "X", k1), "@", unaries, "X", Op.binariable());
		vary2("021d", "@ a X @ 1", EXP, bin(una("@", av), "X", una("@", k1)), "@", unaries, "X", Op.binariable());
		vary2("021c", "@ € a", EXP, una("@", una("€", av)), "@", unaries, "€", unaries);
		vary2("021e", "U a X U b", EXP, bin(una("U", av), "X", una("U", bv)), "U", unaries, "X", Op.binariable());
		taut("021=", "-a", EXP);
		taut("021=", "!a", EXP);
		taut("021=", "1 div 2", EXP);
		taut("021=", "1 mod 2", EXP);
		parsesAs("021-mod-range", "a mod b .. c", EXP, bin(bin(av, MOD, bv), DOTDOT, cv));
	}

	public static String patlistctor(String... bits) {
		return "PatListCtor(bits=[" + sep(bits, ",") + "])";
	}

	public static String patlb(String exp) {
		return "PatListBitExp(pat=" + exp + ")";
	}

	public static String patlbe(String exp) {
		return "PatListBitEllip(pat=" + exp + ")";
	}

	public static String patlit(String v) {
		return "$(" + v + ")";
	}

	public void test022_pat_list() throws Exception {
		parsesAs("022a", "[]", PAT, patlistctor());
		parsesAs("022a", "[1]", PAT, patlistctor(patlb("$(1)")));
		parsesAs("022a", "[1 ...]", PAT, patlistctor(patlbe("$(1)")));
		parsesAs("022a", "[1 …]", PAT, patlistctor(patlbe("$(1)")));
		parsesAs("022a", "[1 ..., x]", PAT, patlistctor(patlbe("$(1)"), patlb(xp)));
		parsesAs("022a", "[1 ..., x, [y]]", PAT, patlistctor(patlbe("$(1)"), patlb(xp), patlb(patlistctor(patlb(yp)))));
		doesntParse("022b", "1+[x]", PAT, NOTPAT);
		doesntParse("022b", "[1+x]", PAT, NOTPAT);
		doesntParse("022b", "[1, 1+x]", PAT, NOTPAT);
		doesntParse("022b", "[1+x, 1]", PAT, NOTPAT);
		doesntParse("022b", "[x, 1, [x+1]]", PAT, NOTPAT);

		parsesAs("022c", "fn [x,y...] = 1", EXP, anonfun(funbody(monobody(formals(patlistctor(patlb(xp), patlbe(yp))),
				k1))));
		parsesAs("022c", "fun f [x,y...] = x + f(y);", STMT, fundecl(f, funbody(monobody(f, formals(patlistctor(
				patlb(xp), patlbe(yp))), bin(xv, PLUS, fc(fv, yv))))));

		taut("022=", "[a, b…]", PAT);
		taut("022=", "[a, b…]", EXP);

		parsesAs("022s", "'x'", PAT, patlit("x"));
		parsesAs("022t", "'x'=1;", STMT, bind(patlit("x"), k1));
	}

	public static String anonobj(List<String> exts, String... members) {
		return AnonObj.detstr(delist(exts), dearr(members), false);
	}

	public static String anonobjVal(List<String> exts, String... members) {
		return AnonObj.detstr(delist(exts), dearr(members), true);
	}

	public static String ext(String qn, String... args) {
		return "ClsExtends(superName=" + qn(qn) + " args=[" + sep(args, ",") + "])";
	}

	public static String def(String name, String funbody) {
		return def(name, funbody, false);
	}

	public static String def(String name, String funbody, boolean purity) {
		return MethDecl.detstr(name, funbody, purity);
	}

	public static String def(String name, String args, String body) {
		return def(name, args, body, false);
	}

	public static String def(String name, String args, String body, boolean purity) {
		return def(name, funbody(purity, monobody(name, (args), body, purity)));
	}

	public String membind(String pat, String to) {
		return bind(pat, to);
	}

	public String memvar(String id, String to) {
		return var(id, to);
	}
	
	
	public String classFormal(String id) {return classformal(id, false, tycon());}
	public String classformal(String id, boolean isvar, String typecons) {
		return ClassFormal.detstr(id, isvar, typecons);
	}

	public String cf_x = classFormal(x);
	public String cf_y = classFormal(y);
	
	
	public void test023_anon_obj_w_member_fun() throws Exception {
		try {
			TestUtils.NO_CLASS_DESUGARING_SO_I_CAN_DO_PARSER_TESTS = true;

			parsesAs("023a", "object{}", EXP, anonobj(NO));
			parsesAs("023a", "object{}object", EXP, anonobj(NO));
			parsesAs("023b", "object extends a() {}", EXP, anonobj(list(ext(a))));
			parsesAs("023c", "object extends a {}", EXP, anonobj(list(ext(a))));
			parsesAs("023d", "object extends a, b {}", EXP, anonobj(list(ext(a), ext(b))));
			String cod1 = "def m() = 1;";
			String mem1 = def(m, formals(), k1);
			parsesAs("023e", "object {" + cod1 + "}", EXP, anonobj(NO, mem1));
			parsesAs("023f", "object extends a(s(1)){def m(x)=x; def g(x,y)={x;y;}}", EXP, anonobj(list(ext(a, fc(sv,
					k1))), def(m, formals(xp), xv), def(g, formals(xp, yp), seq(xv, yv))));
			parsesAs("023g", "object extends a.b.c{}", EXP, anonobj(list(ext(comma(a, b, c)))));
			parsesAs("023h", "object extends a.b.c(1,2){}", EXP, anonobj(list(ext(comma(a, b, c), k1, k2))));
			parsesAs("023h", "object extends a.b.c(1,2), d.e(3){}", EXP, anonobj(list(ext(comma(a, b, c), k1, k2), ext(
					comma(d, e), k3))));

			parsesAs("023i", "object{x=1;}", EXP, anonobj(NO, membind(xp, k1)));
			parsesAs("023i", "object{x=1;}object", EXP, anonobj(NO, membind(xp, k1)));
			parsesAs("023i", "object{x=object{x=1;};}", EXP, anonobj(NO, membind(xp, anonobj(NO, membind(xp, k1)))));
			parsesAs("023i", "object{x= fn 1 = 1;}", EXP, anonobj(NO, membind(xp, anonfun(formals("$(1)"), k1))));
			parsesAs("023i", "object{[x,x]=1;}", EXP, anonobj(NO, membind(patlistctor(patlb(xp), patlb(xp)), k1)));

			parsesAs("023iv", "object{val x=1;}", EXP, anonobj(NO, membind(xp, k1)));
			parsesAs("023iv", "object{val x=object{x=1;};}", EXP,
					anonobj(NO, membind(xp, anonobj(NO, membind(xp, k1)))));
			parsesAs("023iv", "object{val x= fn 1 = 1;}", EXP, anonobj(NO, membind(xp, anonfun(formals("$(1)"), k1))));
			parsesAs("023iv", "object{val [x,x]=1;}", EXP, anonobj(NO, membind(patlistctor(patlb(xp), patlb(xp)), k1)));

			parsesAs("023j", "object{var x;}", EXP, anonobj(NO, memvar(x, no)));
			//parsesAs("023j", "object{var x = 1;}", EXP, anonobj(NO, memvar(x, k1)));
			parsesAs("023j", "object{var x := 1;}", EXP, anonobj(NO, memvar(x, k1)));
			parsesAs("023j", "object{var x := fn x = x;}", EXP, anonobj(NO, memvar(x, anonfun(formals(xp), xv))));

			taut("023=", "object{x = 1;}", EXP);

			doesntParse("023k", "object{new(){}}", EXP, "\"new\"");
		} finally {
			TestUtils.NO_CLASS_DESUGARING_SO_I_CAN_DO_PARSER_TESTS = false;
		}
	}

	public static String clsdecl(boolean isval, String name,  List<String> params, List<String> exts, String... members) {
		return clsdecl(isval, name, ! params.isEmpty(), params, exts, members);
	}
	public static String clsdecl(boolean isval, String name, boolean hasparams, List<String> params, List<String> exts, String... members) {
		return ClsDecl.detstr(isval, name, hasparams, "["+sep(params, ",")+"]", "["+sep(exts, ",")+"]", "["+sep(members, ",")+"]");
//		return "ClsDecl(isMarkedPure=" + isval + " name=" + name + " formals=" + formals + " exts=["
//				+ sep(extendsses, ",") + "] members=[" + sep(members, ",") + "])";
	}

	public void test024_class() throws Exception {
		try {
			TestUtils.NO_CLASS_DESUGARING_SO_I_CAN_DO_PARSER_TESTS = true;
			/* With classes generating stuff, these tests all need revision. 
			 * I'm not exactly sure how to do the revision nicely, since the 
			 * generated stuff is full of gensyms (which don't come out the same every time)
			 * and sets (which don't come out in the same order every time).
			 * But the parser tests are less crucial than later ones,
			 * so I just put in a switch to turn off the desugaring so I can do this test.
			 * */
			parsesAs("024a", "class c{}", CLS, clsdecl(false, c, NO, NO));
			parsesAs("024a2", "class c;", CLS, clsdecl(false, c, NO, NO));
			parsesAs("024a3", "class c{}class", CLS, clsdecl(false, c, NO, NO));
			parsesAs("024a4", "class c{}c", CLS, clsdecl(false, c, NO, NO));
			List<String> extends_d = list(comma(ext(d)));
			parsesAs("024a5", "class c extends d{}", CLS, clsdecl(false, c, NO, extends_d));
			String binds_x_to_1 = membind(xp, k1);
			parsesAs("024a6", "class c extends d{x=1;}", CLS, clsdecl(false, c, NO, extends_d, binds_x_to_1));
			parsesAs("024a7", "class c(x) extends d{x=1;}", CLS, clsdecl(false, c, list(cf_x), extends_d, binds_x_to_1));
			String extends_d_and = ext(d, xv);
			parsesAs("024a8", "class c(x) extends d(x) {x=1;}", CLS, clsdecl(false, c, list(cf_x),
					list(comma(extends_d_and)), binds_x_to_1));
			parsesAs("024a9", "class c(x) extends d(x),e(x) {x=1;}", CLS, clsdecl(false, c, list(cf_x), list(comma(
					extends_d_and, ext(e, xv))), binds_x_to_1));
			parsesAs("024aa", "class c(x) extends d(x),f.e(x) {x=1;}", CLS, clsdecl(false, c, list(cf_x), list(comma(
					extends_d_and, ext(comma(f, e), xv))), binds_x_to_1));
			parsesAs("024ab", "class  c(x) : pure extends d(x),f.e(x) {x=1;}", CLS, clsdecl(true, c, list(cf_x),
					list(comma(extends_d_and, ext(comma(f, e), xv))), binds_x_to_1));
			parsesAs("024ax", "class c(x) : pure extends d(x),f.e(x) {x=1; def m()=2; def m(x){y;z;}}", CLS, clsdecl(
					true, c, list(cf_x), list(comma(extends_d_and, ext(comma(f, e), xv))), binds_x_to_1, def(m,
							formals(), k2), def(m, formals(xp), seq(yv, zv))));
			parsesAs("024ac", "class c(x) : pure extends d(x),f.e(x) {x=1; def m()=2; def m(x){y;z;}}class", CLS,
					clsdecl(true, c, list(cf_x), list(comma(extends_d_and, ext(comma(f, e), xv))), binds_x_to_1, def(m,
							formals(), k2), def(m, formals(xp), seq(yv, zv))));
			parsesAs("024ad", "class  c(x): pure extends d(x),f.e(x) {x=1; def m()=2; def m(x){y;z;}}c", CLS, clsdecl(
					true, c, list(cf_x), list(comma(extends_d_and, ext(comma(f, e), xv))), binds_x_to_1, def(m,
							formals(), k2), def(m, formals(xp), seq(yv, zv))));

			parsesAs("024b", "class c(x,y) extends d.e(x,y){ var v := 1;}", CLS, clsdecl(false, c, list(cf_x, cf_y),
					list(ext(comma(d, e), comma(xv, yv))), memvar(v, k1)));

			taut("024=", "class c(x, y) extends d.e(x,y){var v := 1;}", CLS);
			taut("024=", "class c(x, y) extends d.e(x,y){var v := 1; def f(x) = x;}", CLS);

			doesntParse("024c", "class C(x,y){}cls", CLS, "equal to class or C");

			parsesAs("024m", "class c{x;}", CLS, clsdecl(false, c, NO, NO, membind(xp, no)));
			parsesAs("024m", "class c{+x;}", CLS, clsdecl(false, c, NO, NO, membind(patplus(xp), no)));
			parsesAs("024m", "class c{val x;}", CLS, clsdecl(false, c, NO, NO, membind(xp, no)));

			parsesAs("024n", "class c{import a.b.*;}", CLS, clsdecl(false, c, NO, NO, impor(qn(a, b), NO, no, true)));
			
			parses("024o", "class A extends B(,){}", CLS);
			parses("024o", "class A extends B(x,){}", CLS);
			parses("024o", "class A extends B(x,y,){}", CLS);
			

		} finally {
			TestUtils.NO_CLASS_DESUGARING_SO_I_CAN_DO_PARSER_TESTS = false;
		}

	}

	public static String ctor(String id, String formals, String body) {
		return ctor(id, monobody(id, formals, body));
	}

	public static String ctor(String id, String monobody) {
		return "ClsCtorDef(id=" + id + " monobody=" + monobody + ")";
	}

	public void test025_ctor() throws Exception {
		parsesAs("025a", "class c{new c()={}}", CLS, clsdecl(false, c, NO, NO, ctor(c, formals(), seq())));
		parsesAs("025a", "class c{new c(){};}", CLS, clsdecl(false, c, NO, NO, ctor(c, formals(), seq())));
		parsesAs("025a", "class c{new c()={};}", CLS, clsdecl(false, c, NO, NO, ctor(c, formals(), seq())));
		parsesAs("025a", "class c{new c(){}}", CLS, clsdecl(false, c, NO, NO, ctor(c, formals(), seq())));
		parsesAs("025b", "class c{new c(x){}}", CLS, clsdecl(false, c, NO, NO, ctor(c, formals(xp), seq())));
		parsesAs("025b", "class c{new c(x){y=x;}}", CLS, clsdecl(false, c, NO, NO, ctor(c, formals(xp),
				seq(bind(yp, xv)))));
		parsesAs("025b", "class c{new c(x,y){y=x;}}", CLS, clsdecl(false, c, NO, NO, ctor(c, formals(xp, yp), seq(bind(
				yp, xv)))));

		parsesAs("025c", "class c{new ()={}}", CLS, clsdecl(false, c, NO, NO, ctor(no, formals(), seq())));
		parsesAs("025d", "class c{new (){};}", CLS, clsdecl(false, c, NO, NO, ctor(no, formals(), seq())));
		parsesAs("025e", "class c{new ()={};}", CLS, clsdecl(false, c, NO, NO, ctor(no, formals(), seq())));
		parsesAs("025f", "class c{new (){}}", CLS, clsdecl(false, c, NO, NO, ctor(no, formals(), seq())));
		parsesAs("025g", "class c{new (x){}}", CLS, clsdecl(false, c, NO, NO, ctor(no, formals(xp), seq())));
		parsesAs("025h", "class c{new (x){y=x;}}", CLS, clsdecl(false, c, NO, NO, ctor(no, formals(xp),
				seq(bind(yp, xv)))));
		parsesAs("025i", "class c{new (x,y){y=x;}}", CLS, clsdecl(false, c, NO, NO, ctor(no, formals(xp, yp), seq(bind(
				yp, xv)))));

		parsesAs("025g", "class c{new [x] {}}", CLS, clsdecl(false, c, NO, NO, ctor(no,
				formals(patlistctor(patlb(xp))), seq())));
		parsesAs("025g", "class c{new c[x] {}}", CLS, clsdecl(false, c, NO, NO, ctor(c,
				formals(patlistctor(patlb(xp))), seq())));
		parsesAs("025g", "class c{new c[x] {}new}", CLS, clsdecl(false, c, NO, NO, ctor(c,
				formals(patlistctor(patlb(xp))), seq())));

		taut("025=", "class c{new c(x) = {}}", CLS);
	}

	public static String patdef(String id, List<String> formals, String where, String body) {
		return ClsPatDef.detstr(id, delist(formals), body);
		//		return "ClsPatDef(id=" + id + " formals=[" + sep(formals, ",") + "] where=" + where + " body=" + body +")";
	}

//	public void test026_pat() throws Exception {
//		parsesAs("026a", "class c{pat p(){}}", CLS, clsdecl(false, c, NO, NO, patdef(p, NO, no, seq())));
//		parsesAs("026a", "class c{pat p(){}pat}", CLS, clsdecl(false, c, NO, NO, patdef(p, NO, no, seq())));
//		parsesAs("026a", "class c{pat p(){}p}", CLS, clsdecl(false, c, NO, NO, patdef(p, NO, no, seq())));
//		parsesAs("026b", "class c{pat p(a,b,c){}}", CLS, clsdecl(false, c, NO, NO, patdef(p, list(a, b, c), no, seq())));
//		parsesAs("026c", "class c{pat p(a,b,c){a=1;}}", CLS, clsdecl(false, c, NO, NO, patdef(p, list(a, b, c), no,
//				seq(bind(ap, k1)))));
//		taut("026d", "class c{pat p() = {x = 2;}}", CLS);
//
//		parses("026z", "class c{pat p(){}p  pat q(){}pat }c", CLS);
//	}

	public static String modarg(String formal, String actual) {
		return ModArgBinding.detstr(formal, actual);
	}

	public static String importOwn(String modQn, List<String> genArgs, String as, boolean dotstar) {
		return ImportStmt.detstr("true", modQn, delist(genArgs), as, dotstar);
	}

	public static String impor(String modQn, List<String> genArgs, String as, boolean dotstar) {
		return ImportStmt.detstr("false", modQn, delist(genArgs), as, dotstar);
	}

	public void OLD_test027_import() throws Exception {
		parsesAs("027a", "import a;", STMT, impor(qn(a), NO, no, false));
		parsesAs("027a", "import own a;", STMT, importOwn(qn(a), NO, no, false));
		parsesAs("027b", "import a(x=a);", STMT, impor(qn(a), list(modarg(x, qn(a))), no, false));
		parsesAs("027c", "import a as q.r;", STMT, impor(qn(a), NO, qn(q, r), false));

		parsesAs("027d", "import a.b.c (y=c.b.a, z=w) as r.s;", STMT, impor(qn(a, b, c), list(modarg(y, qn(c, b, a)),
				modarg(z, qn(w))), qn(r, s), false));
		taut("027d", "import a.b.c (y=c.b.a, z=w) as r.s;", STMT);
		taut("027e", "import a.*;", STMT);
		parsesAs("027f", "import a.*;", STMT, impor(qn(a), NO, no, true));
		parsesAs("027g", "import a.b.c.* as d;", STMT, impor(qn(a, b, c), NO, qn(d), true));
	}

	public void test027_import_revised() throws Exception {
		parsesAs("027a", "import a;", STMT, impor(qn(a), NO, no, false));
		parsesAs("027a", "import own a;", STMT, importOwn(qn(a), NO, no, false));
		parsesAs("027b", "import a(b=c);", STMT, impor(qn(a), list(modarg(b, qn(c))), no, false));
		parsesAs("027c", "import a = q.r;", STMT, impor(qn(q, r), NO, a, false));

		parsesAs("027d", "import a = a.b.c (d=q.r, e=s);", STMT, impor(qn(a, b, c), list(modarg(d, qn(q, r)), modarg(e,
				qn(s))), a, false));
		taut("027d", "import x = a.b.c (A=a, BEE=b, SEA=c);", STMT);
		taut("027e", "import a.*;", STMT);
		parsesAs("027f", "import a.*;", STMT, impor(qn(a), NO, no, true));
		parsesAs("027g", "import d= a.b.c.*;", STMT, impor(qn(a, b, c), NO, d, true));
	}

	public static String alias(String oldqn, String newqn) {
		return Alias.detstr(oldqn, newqn);
	}

	public void test028_alias() throws Exception {
		parsesAs("028a", "alias a = a;", STMT, alias(qn(a), qn(a)));
		parsesAs("028a", "alias a.b.c = a.e.f;", STMT, alias(qn(a, b, c), qn(a, e, f)));
	}

	public void test029_op_cont() throws Exception {
		parsesAs("029a", "1 .. 2", EXP, bin(k1, DOTDOT, k2));
		parsesAs("029a", "1 ..< 2", EXP, bin(k1, DOTDOTLT, k2));
		parsesAs("029b", "a = 1", EXP, bind(ap, k1));
		parsesAs("029c", "a = 1 .. 2", EXP, bind(ap, bin(k1, DOTDOT, k2)));
		parsesAs("029c", "a = 4 in 1 .. 2 + 3", EXP, bind(ap, comp(k4, IN, bin(k1, DOTDOT, bin(k2, PLUS, k3)))));
		parsesAs("029c", "a = 4+a in 1 .. 2 + 3", EXP, bind(ap, comp(bin(k4, PLUS, av), IN, bin(k1, DOTDOT, bin(k2,
				PLUS, k3)))));
		parsesAs("029c", "a = 4+a*b in 1 .. 2 + 3", EXP, bind(ap, comp(bin(k4, PLUS, bin(av, TIMES, bv)), IN, bin(k1,
				DOTDOT, bin(k2, PLUS, k3)))));
	}

	public static String dearr(String[] stuff) {
		return "[" + sep(stuff, ",") + "]";
	}

	//	public static String table(String... bod) {
	//		return Table.detstr(dearr(bod));
	//	}
	//	
	//	
	//	
	//	public static String ioi(String id, ColAccess colAccess) {
	//		return IdWithOptInit.detstr(id, no, colAccess);
	//	}
	//	
	//	public static String ioi(String id, String init, ColAccess colAccess) {
	//		return IdWithOptInit.detstr(id, init, colAccess);
	//	}
	//	
	//	public static String tabfl(ColAccess ack, String ... vars) {
	//		return TableFields.detstr(ack.text, ColSpecial.NORMAL.name, dearr(vars));
	//	}
	//	
	//	public static String tabfl_map(ColAccess ack, String ... vars) {
	//		return TableFields.detstr(ack.text, ColSpecial.MAP.name, dearr(vars));
	//	}
	//	

	public static String map() {
		return MapCtor.detstr();
	}

	public void test030_table() throws Exception {
		//		parsesAs("030a", "table{}", EXP, table());
		//		parsesAs("030b", "table{a;}", EXP, table(tabfl(VAL, ioi(a, VAL))));
		//		parsesAs("030bx", "table{a;}table", EXP, table(tabfl(VAL, ioi(a, VAL))));
		//		parsesAs("030b", "table{val a;}", EXP, table(tabfl(VAL, ioi(a, VAL))));
		//		parsesAs("030by", "table{var a;}", EXP, table(tabfl(VAR, ioi(a, VAR))));
		//		parsesAs("030b", "table{key a;}", EXP, table(tabfl(KEY, ioi(a, KEY))));
		//		doesntParse("030c2", "table{table var a = 1;}", EXP, "Encountered \"=\"");
		//		doesntParse("030c", "table{table val a := 1", EXP, "Encountered \":=\"");
		//		
		//		String funf = (def(f, formals(), k1));
		//		parsesAs("030d", "table{def f() = 1;}", EXP, table(funf));
		//		parsesAs("030d", "table{def f() = 1; def f() = 1;}", EXP, table(funf, funf));
		//		
		//		parsesAs("030e", "table{val a,b;}", EXP, table(tabfl(VAL, ioi(a, VAL), ioi(b, VAL))));
		//		parsesAs("030e", "table{a,b;}", EXP, table(tabfl(VAL, ioi(a, VAL), ioi(b, VAL))));
		//		parsesAs("030e", "table{var a,b;}", EXP, table(tabfl(VAR, ioi(a, VAR), ioi(b, VAR))));
		//		parsesAs("030e", "table{key a,b;}", EXP, table(tabfl(KEY, ioi(a, KEY), ioi(b, KEY))));
		//		parsesAs("030e", "table{key a,b;def f() = 1;}", EXP, table(tabfl(KEY, ioi(a, KEY), ioi(b, KEY)), funf));
		//		
		//		parsesAs("030f", "table{var a,b; key c,d;}", EXP, table(tabfl(VAR, ioi(a, VAR), ioi(b, VAR)), tabfl(KEY, ioi(c, KEY),ioi(d, KEY))));
		//		
		//		taut("030f", "table{var a, b; key c, d;}", EXP);
		//		
		//		parsesAs("030g", "table{key a; map b;}", EXP, table(tabfl(KEY, ioi(a, KEY)), tabfl_map(VAL, ioi(b, VAL))));
		//		
		//		taut("030h", "table{key a; map val b;}", EXP);
		//		taut("030h", "table{map key a; map val b;}", EXP);
		//		
		//		taut("030i", "map{}", EXP);
		//		taut("030i", "map{1+1 => 2+2, 3 => 4}", EXP);
		//		
		//		parsesAs("030j", "map{}", EXP, map());
		//		parsesAs("030j", "map{1=>2}", EXP, map(kv(k1,k2)));
		//		parsesAs("030j", "map{1=>2, 1+1 => 1*1}", EXP, map(kv(k1,k2), kv(bin(k1, PLUS, k1), bin(k1, TIMES, k1))));
		//		
		taut("030xx", "{t(1).b := 2;}", STMT);
	}

	public static String module(Boolean isval, String modulename, String... modulebits) {
		return Module.detstr(isval.toString(), modulename, dearr(modulebits));
	}

	public static String modimp(String impor) {
		return ModuleFileImport.detstr(impor);
	}

	public static String modvis(Visibility vis, String id) {
		return ModuleFileVisibility.detstr(vis.toString(), id);
	}

	public static String modali(String ali) {
		return ModuleFileAlias.detstr(ali);
	}

	public static String modmem(String fn) {
		return ModuleFileMemberStmt.detstr(fn);
	}

	public void test031_module_file() throws Exception {
		parsesAs("031a", "module m{}", MODU, module(false, qn(m)));
		parsesAs("031a", "module m{}m", MODU, module(false, qn(m)));
		parsesAs("031a", "module m{}module", MODU, module(false, qn(m)));
		parsesAs("031b", "val module m{alias a =b;}", MODU, module(true, qn(m), modali(alias(qn(a), qn(b)))));
		parsesAs("031c", "module m{public h;}", MODU, module(false, qn(m), modvis(Visibility.PUB, h)));
		parsesAs("031c", "module m{public h; private h;}", MODU, module(false, qn(m), modvis(Visibility.PUB, h),
				modvis(Visibility.PRIV, h)));
		parsesAs("031e", "module d.u.l.l{member \"blerg/$x\";}", MODU,
				module(false, qn(d, u, l, l), modmem("blerg/$x")));
		parsesAs("031f", "module  b.l.a{import f = a.s.s(b=c.o.d,d=e.l.f);}", MODU, module(false, qn(b, l, a),
				modimp(impor(qn(a, s, s), list(modarg(b, qn(c, o, d)), modarg(d, qn(e, l, f))), f, false))));
		parsesAs("031g", "module a{class b{}}", MODU, module(false, qn(a), clsdecl(false, b, NO, NO)));
		parsesAs("031h", "module a{class b{} fun c()=d; var e := f;}", MODU, module(false, qn(a), clsdecl(false, b, NO,
				NO), fundecl(c, "", dv), var(e, fv)));
		parsesAs("031i", "module a{val v = 1; w = 2;}", MODU, module(false, qn(a), bind(vp, k1), bind(wp, k2))

		);
	}

	public void test033_break_continue() throws Exception {
		parsesAs("033a", "break;", STMT, Break.detstr(null));
		parsesAs("033a", "break b;", STMT, Break.detstr(b));
		parsesAs("033a", "continue;", STMT, Continue.detstr(null));
		parsesAs("033a", "continue pain;", STMT, Continue.detstr("pain"));
	}

	private void for_test_034(String locator, List<String> L) {
		FisherSource source = new FisherSource.FromString(sep(L, "\n"));
		final int N = L.size();
		for (int ij = 0; ij < N; ij++) {
			int ic = ij + 1; // line number from JavaCC
			for (int jj = ij; jj < N; jj++) {
				int jc = jj + 1; // line number from JavaCC
				String desired = sep(L.subList(ij, jj + 1), "\n");
				String actual = source.betweenLinesOf(ic, jc);
				assertEquals(locator + "[i=" + ic + "; j=" + jc + "]", desired, actual);
			}
		}
	}

	public void test034_source_line_grabbing() throws Exception {
		String as = "a is for alxobear"; // 1
		String bs = "b is for bluxion"; // 2
		String cs = "c is for cterion"; // 3
		String ds = "d is for dunderpoop"; // 4
		String es = "e is for eltlpl"; // 5
		String fs = "f is for fborking"; // 6
		String gs = "g is for glunzr"; // 7
		String hs = "h is for hulp"; // 8
		List<String> strings = list(as, bs, cs, ds, es, fs, gs, hs);

		for_test_034("034-a", strings);
		for_test_034("034-b", list(as));
		for_test_034("034-b", list(as, ""));
		for_test_034("034-b", list(as, "", ""));
		for_test_034("034-b", list(as, "", "", bs));
		for_test_034("034-b", list(as, "", "", bs, ""));
		for_test_034("034-b", list(a, "", "", bs, ""));
		for_test_034("034-b", list(a, "", "", bs, "", c));
	}

	public static String IFE(String test, String Then, String Else) {
		return If.detstr(test, Then, Else, "false", "true");
	}

	public void test035_cond_exp() throws Exception {
		String ife123 = IFE(k2, k1, k3);
		parsesAs("035a", "1 if 2 else 3", EXP, ife123);
		parsesAs("035b", "1 if 2 else 3 if a else b", EXP, IFE(k2, k1, IFE(av, k3, bv)));
		parsesAs("035c", "a = 1 if 2 else 3", EXP, bind(ap, ife123));
		parsesAs("035c", "a = 1 if 2 else 3;", STMT, bind(ap, ife123));
		parsesAs("035d", "1+2*3 if a<b || c>d else e.f", EXP, IFE(varop(OR, comp(av, LT, bv), comp(cv, GT, dv)), bin(
				k1, PLUS, bin(k2, TIMES, k3)), fr(ev, f)));
		parsesAs("035e", "fn(x)= (x if x>0 else 2)", EXP, anonfun(formals(xp), parens(IFE(comp(xv, GT, k0), xv, k2))));
		parsesAs("035f", "(fn(x)= x) if x>0 else 2", EXP, IFE(comp(xv, GT, k0), parens(anonfun(formals(xp), xv)), k2));
		doesntParse("035g", "fn(x)= x if x>0 else 2", EXP, "Encountered \"if\"");
	}

	public static String patwild() {
		return PatWildcard.detstr();
	}

	public static String patinterp(String exp) {
		return PatInterpolation.detstr(exp);
	}

	public static String it() {
		return ItExp.detstr();
	}

	public static String pattest(String exp) {
		return PatEvalTestExp.detstr(exp);
	}

	public static String patand(String... strings) {
		return PatAnd.detstr(dearr(strings));
	}

	public static String pator(String... strings) {
		return PatOr.detstr(dearr(strings));
	}

	public static String patnot(String str) {
		return PatNot.detstr(str);
	}

	public static String patplus(String str) {
		return PatNotNull.detstr(str);
	}

	public void test036_more_patterns() throws Exception {
		parsesAs("036a", "_ = 1", EXP, bind(patwild(), k1));
		parsesAs("036b", "[_] = 1", EXP, bind(patlistctor(patlb(patwild())), k1));
		parsesAs("036c", "[_,x] = 1", EXP, bind(patlistctor(patlb(patwild()), patlb(xp)), k1));

		parsesAs("036d", "$(1) = a", EXP, bind(patinterp(k1), av));
		parsesAs("036d", "$(x+y) = a", EXP, bind(patinterp(bin(xv, PLUS, yv)), av));
		parsesAs("036e", "[x, $(x)] = a", EXP, bind(patlistctor(patlb(xp), patlb(patinterp(xv))), av));

		parsesAs("036g", "[x, (f(it))?] = a", EXP, bind(patlistctor(patlb(xp), patlb(pattest(fc(fv, it())))), av));

		parsesAs("036h", "(1+2)? = a", EXP, bind(pattest(bin(k1, PLUS, k2)), av));
		// Can we nest stuff?
		// Nest nest nest!

		parsesAs("036j", "x && y = 1", EXP, bind(patand(xp, yp), k1));
		parsesAs("036j", "x || y = 1", EXP, bind(pator(xp, yp), k1));
		parsesAs("036j", "!x = 1", EXP, bind(patnot(xp), k1));
		parsesAs("036k", "!(a)? = 1", EXP, bind(patnot(pattest(av)), k1));
		parsesAs("036k", "!((a)?) = 1", EXP, bind(patnot(pattest(av)), k1));

		doesntParse("036k", "x && y || z = 1", EXP, "Encountered");

		parsesAs("036l", "_ && 1 && x && [x,y] = 2", EXP, bind(patand(patwild(), "$(1)", xp, patlistctor(patlb(xp),
				patlb(yp))), k2));
		parsesAs("036l", "_ || 1 || x || [x,y] = 2", EXP, bind(pator(patwild(), "$(1)", xp, patlistctor(patlb(xp),
				patlb(yp))), k2));
		parsesAs("037m", "+x = 1;", STMT, bind(patplus(xp), k1));
		parsesAs("037m", "+!x = 1;", STMT, bind(patplus(patnot(xp)), k1));
		parsesAs("037m", "+!+x = 1;", STMT, bind(patplus(patnot(patplus(xp))), k1));
		parsesAs("037n", "+(x && y) = 1;", STMT, bind(patplus(patand(xp, yp)), k1));
		parsesAs("037o", "(1~x || 2~x) = 1;", STMT, bind(pator(patilde(k1, xp), patilde(k2, xp)), k1));
	}

	public void test037_bad_pats() throws Exception {
		doesntParse("037a", "x*y = z", EXP, NOTPAT);
		doesntParse("037b", "- x = z", EXP, NOTPAT);
		doesntParse("037b", "+ - x = z", EXP, NOTPAT);
		doesntParse("037b", "+ (x * y) = z", EXP, NOTPAT);
		doesntParse("037b", "x + 1 = z", EXP, NOTPAT);
	}

	public static String tilde(String exp, String pat) {
		return MatchExp.detstr(exp, pat);
	}

	public static String patilde(String exp, String pat) {
		return PatMatchSomethingElse.detstr(exp, pat);
	}

	public void test038_tilde() throws Exception {
		parsesAs("038a", "1~a", EXP, tilde(k1, ap));
		parsesAs("038a", "1~a && b", EXP, varop(AND, tilde(k1, ap), bv));
		parsesAs("038a", "1~(a && b)", EXP, tilde(k1, patand(ap, bp)));
		parsesAs("038b", "1+2 ~ +a", EXP, tilde(bin(k1, PLUS, k2), patplus(ap)));
		parsesAs("038b", "1<2 ~ +a", EXP, tilde(comp(k1, LT, k2), patplus(ap)));
		parsesAs("038b", "1<2 ~ (it<it)?", EXP, tilde(comp(k1, LT, k2), pattest(comp(it(), LT, it()))));

		parsesAs("038c", "1~a && 2~b", EXP, varop(AND, tilde(k1, ap), tilde(k2, bp)));
		// And for some patterns which have ~'s ...

		parsesAs("038d", "1~a", PAT, patilde(k1, ap));
		parsesAs("038e", "1~a && b", PAT, patand(patilde(k1, ap), bp));
		parsesAs("038f", "1~(a && b)", PAT, patilde(k1, patand(ap, bp)));
		parsesAs("038g", "1+2 ~ +a", PAT, patilde(bin(k1, PLUS, k2), patplus(ap)));
		parsesAs("038h", "1<2 ~ +a", PAT, patilde(comp(k1, LT, k2), patplus(ap)));
		parsesAs("038i", "1<2 ~ (it<it)?", PAT, patilde(comp(k1, LT, k2), pattest(comp(it(), LT, it()))));

		parsesAs("038j", "1~a && 2~b", PAT, patand(patilde(k1, ap), patilde(k2, bp)));
	}

	public static String This() {
		return fisher.syn.This.detstr();
	}

	public void test039_literals_and_this_and_such() throws Exception {
		parsesAs("039a", "true", EXP, "(true)");
		parsesAs("039a", "false", EXP, "(false)");
		parsesAs("039a", "null", EXP, "(<null>)");
		parsesAs("039a", "this", EXP, This());

	}

	public static String probe(String id, int n, String... cmds) {
		return Probe.detstr(id, dearr(cmds), n);
	}

	public static final int NOPRO = -1;

	public void test040_probe() throws Exception {
		parsesAs("040a", "~!@()", EXP, probe(no, NOPRO));
		parsesAs("040b", "~!@f()", EXP, probe(f, NOPRO));
		parsesAs("040c", "~!@f(a,b+1)", EXP, probe(f, NOPRO, av, bin(bv, PLUS, k1)));
		parsesAs("040c", "~!@(a,b+1)", EXP, probe(no, NOPRO, av, bin(bv, PLUS, k1)));
		parsesAs("040d", "~!@f()@!~3", EXP, probe(f, 3));
		parsesAs("040d", "~!@f()@!~", EXP, probe(f, 1));
	}

	public static String count(String pred, String... controls) {
		return QueryQuantifierCount.detstr(dearr(controls), pred);
	}

	public static String every(String pred, String... controls) {
		return QueryQuantifierEvery.detstr(dearr(controls), pred);
	}

	public static String some(String pred, String... controls) {
		return QueryQuantifierSome.detstr(dearr(controls), pred);
	}

	public static String qcfor(String pat, String list, boolean inquisitive) {
		return QueryControlFor.detstr(pat, list, inquisitive);
	}

	public static String qcif(String pred) {
		return QueryControlIf.detstr(pred);
	}

	public void test041_count() throws Exception {
		parsesAs("041a", "%count(1 | for x <- 2)", EXP, count(k1, qcfor(xp, k2, false)));
		parsesAs("041a", "%count(1 | for x <~ 2)", EXP, count(k1, qcfor(xp, k2, true)));
		parsesAs("041a", "%count(1 | for x <- 2, for [y,z] <- 3)", EXP, count(k1, qcfor(xp, k2, false), qcfor(
				patlistctor(patlb(yp), patlb(zp)), k3, false)));
		taut("041c", "%count(x | for y <- z)", EXP);
		taut("041c", "%count(x | for y <- z, for a <- b)", EXP);

		parsesAs("041a", "%every(1 | for x <- 2)", EXP, every(k1, qcfor(xp, k2, false)));
		parsesAs("041a", "%every(1 | for x <- 2, for [y,z] <- 3)", EXP, every(k1, qcfor(xp, k2, false), qcfor(
				patlistctor(patlb(yp), patlb(zp)), k3, false)));
		taut("041c", "%every(x | for y <- z)", EXP);
		taut("041c", "%every(x | for y <- z, for a <- b)", EXP);

		parsesAs("041a", "%some(1 | for x <- 2)", EXP, some(k1, qcfor(xp, k2, false)));
		parsesAs("041a", "%some(1 | for x <- 2, for [y,z] <- 3)", EXP, some(k1, qcfor(xp, k2, false), qcfor(
				patlistctor(patlb(yp), patlb(zp)), k3, false)));
		taut("041c", "%some(x | for y <- z)", EXP);
		taut("041c", "%some(x | for y <~ z)", EXP);
		taut("041c", "%some(x | for y <- z, for a <- b)", EXP);

		parsesAs("041d", "%count(1 | if a)", EXP, count(k1, qcif(av)));
		parsesAs("041d", "%count(1 | if a, for b <- c)", EXP, count(k1, qcif(av), qcfor(bp, cv, false)));
		parsesAs("041e", "%count(1 | if a, if 1>1)", EXP, count(k1, qcif(av), qcif(comp(k1, GT, k1))));

		parsesAs("041f", "%count(%count(1 | if a) | for y <- z)", EXP, count(count(k1, qcif(av)), qcfor(yp, zv, false)));
		taut("041g", "%some(x | if y)", EXP);
	}

	public static String swiss(String one, String more, String less, String... controls) {
		return QuerySwiss.detstr(dearr(controls), one, more, less);
	}

	public static String qcval(String pat, String exp) {
		return QueryControlVal.detstr(pat, exp);
	}

	public static String qcvar0(String var, String init, String next) {
		return QueryControlVar.detstr(var, init, next, "true");
	}

	public static String qcvar1(String var, String init, String next) {
		return QueryControlVar.detstr(var, init, next, false);
	}

	public void test042_swiss() throws Exception {
		String ifb = qcif(bv);
		parsesAs("042a", "%(a | if b)", EXP, swiss(av, no, no, ifb));
		parsesAs("042a", "%(a %< c | if b)", EXP, swiss(av, no, cv, ifb));
		parsesAs("042a", "%(a %< c %> d | if b)", EXP, swiss(av, dv, cv, ifb));
		parsesAs("042a", "%(a %> d %< c  | if b)", EXP, swiss(av, dv, cv, ifb));
		parsesAs("042a", "%(a %> d | if b)", EXP, swiss(av, dv, no, ifb));

		String toomanyless = "Too many %< clauses";
		String toomanymore = "Too many %> clauses";
		doesntParse("042b", "%(a %< b %< c | if d)", EXP, toomanyless);
		doesntParse("042b", "%(a %> b %> c | if d)", EXP, toomanymore);
		doesntParse("042b", "%(a %> b %< c  %> e | if d)", EXP, toomanymore);

		taut("042c", "%(a %> b %< c | if d)", EXP);

		parsesAs("042a", "%(a | if b)", EXP, swiss(av, no, no, ifb));

		parsesAs("042d", "%(a | val b = c)", EXP, swiss(av, no, no, qcval(bp, cv)));
		parsesAs("042d", "%(a | b = c)", EXP, swiss(av, no, no, qcval(bp, cv)));
		parsesAs("042d", "%(a | [b] = c)", EXP, swiss(av, no, no, qcval(patlistctor(patlb(bp)), cv)));
		parsesAs("042d", "%(a | [b] = c, e=f)", EXP,
				swiss(av, no, no, qcval(patlistctor(patlb(bp)), cv), qcval(ep, fv)));

		taut("042e", "%(a | val b = c)", EXP);

		taut("042f", "%(a | var x := 1 %then0 2)", EXP);
		taut("042f", "%(a | var x := 1 %then1 2)", EXP);

		parsesAs("042g", "%(a | var b := c %< d)", EXP, swiss(av, no, no, qcvar0(b, cv, dv)));
		parsesAs("042g", "%(a | var b := c %> d)", EXP, swiss(av, no, no, qcvar1(b, cv, dv)));
	}

	public static String first(String exp, String none, String... controls) {
		return QueryFirst.detstr(dearr(controls), exp, none, true);
	}

	public static String after(String exp, String... controls) {
		return QueryAfter.detstr(dearr(controls), exp);
	}

	public static String qcwhile(String exp, boolean isUntil) {
		return QueryControlWhile.detstr(exp, isUntil);
	}

	public void test043_first_and_last_men() throws Exception {
		taut("043a", "%first(a | if b)", EXP);
		taut("043a", "%after(a | if b)", EXP);
		taut("043a", "%first(a %< b | if b)", EXP);

		parsesAs("043b", "%first(a | if b)", EXP, first(av, no, qcif(bv)));
		parsesAs("043b", "%find(a | if b)", EXP, first(av, no, qcif(bv)));
		parsesAs("043b", "%first(a %< 1 | if b)", EXP, first(av, k1, qcif(bv)));
		parsesAs("043c", "%first(a+b %< 1 | for a <- 1 .. b, if p(a))", EXP, first(bin(av, PLUS, bv), k1, qcfor(ap,
				bin(k1, DOTDOT, bv), false), qcif(fc(pv, av))));
		parsesAs("043b", "%after(a | if b)", EXP, after(av, qcif(bv)));
		parsesAs("043c", "%after(a+b | for a <- 1 .. b, if p(a))", EXP, after(bin(av, PLUS, bv), qcfor(ap, bin(k1,
				DOTDOT, bv), false), qcif(fc(pv, av))));
		parsesAs("043g", "%first(a | while b)", EXP, first(av, no, qcwhile(bv, false)));
		parsesAs("043g", "%first(a | until b)", EXP, first(av, no, qcwhile(bv, true)));
		taut("043h1", "%first(a | while b)", EXP);
		taut("043h2", "%first(a | until b)", EXP);
		taut("043h3", "%first(a | until b, if c)", EXP);
		taut("043i", "first(while b, for c <- d){e();} else {f();}", STMT);

	}

	public static String listcomp(String exp, boolean isAppended, String... controls) {
		return QueryListComprehension.detstr(dearr(controls), exp, isAppended);
	}

	public void test044_query_list_comprehension() throws Exception {
		parsesAs("044a", "%[a | if b]", EXP, listcomp(av, false, qcif(bv)));
		parsesAs("044a", "%[a ... | if b]", EXP, listcomp(av, true, qcif(bv)));
		parsesAs("044a", "%[a +a ... | if b]", EXP, listcomp(bin(av, PLUS, av), true, qcif(bv)));
		taut("044b", "%[a | if b]", EXP);
		parsesAs("044c", "%[ [a, b...] ... | if c]", EXP, listcomp(listctor(lb(av), lbe(bv)), true, qcif(cv)));
		parsesAs("044c", "%[ [a, b…] … | if c]", EXP, listcomp(listctor(lb(av), lbe(bv)), true, qcif(cv)));
	}

	//	public static String tablecomp(List<String> tablebits, String ... controls) {
	//		return QueryTable.detstr(dearr(controls), delist(tablebits));
	//	}

	//	public static String group(List<String> tablebits, String ... controls) {
	//		return QueryGroup.detstr(dearr(controls), delist(tablebits));
	//	}
	//	
	public void test045_table_comprehension() throws Exception {
		//		parsesAs("045a", "%table(key a = i; % for i <- j)", EXP, 
		//				tablecomp(list(tabfl(KEY, ioi(a, iv, KEY))),
		//						qcfor(ip, jv)
		//						)
		//		);
		//		
		//		parsesAs("045b", "%table(key a = i; map b = i*i;  % for i <- j)", EXP, 
		//				tablecomp(list(tabfl(KEY, ioi(a, iv, KEY)), tabfl_map(VAL, ioi(b, bin(iv, TIMES, iv), VAL) )),
		//						qcfor(ip, jv)
		//						)
		//		);
		//		//String funf = tabfun(fundecl(f, "", k1));
		//		parsesAs("045c", "%table(key a = i; map b = i*i; def f(x)=1;  % for i <- j)", EXP, 
		//				tablecomp(list(tabfl(KEY, ioi(a, iv, KEY)), 
		//							   tabfl_map(VAL, ioi(b, bin(iv, TIMES, iv), VAL)),
		//							   (def(f, formals(xp), k1))
		//					     ),
		//						qcfor(ip, jv)
		//						)
		//		);
		//
		//		parsesAs("045d", "%table(key a = i; map b = i*i; def f(x)=1; def s ([x, y...]) = x + s(y); | s([]) = 1; % for i <- j)", EXP, 
		//				tablecomp(list(tabfl(KEY, ioi(a, iv, KEY)), 
		//							   tabfl_map(VAL, ioi(b, bin(iv, TIMES, iv), VAL)),
		//							   (def(f, formals(xp), k1)),
		//							   (def(s, funbody(monobody(s, formals(patlistctor(patlb(xp), patlbe(yp))), bin(xv, PLUS, fc(sv, yv))),
		//									   	monobody(s, formals(patlistctor()), k1))))
		//					     ),
		//						qcfor(ip, jv)
		//						)
		//		);
		//		
		//		parsesAs("045e", "%table{key a = i; % for i <- j}", EXP, 
		//				tablecomp(list(tabfl(KEY, ioi(a, iv, KEY))),
		//						qcfor(ip, jv)
		//				)
		//		);
		//		parsesAs("045f", "%table{key a = i; % for i <- j}table", EXP, 
		//				tablecomp(list(tabfl(KEY, ioi(a, iv, KEY))),
		//						qcfor(ip, jv)
		//						)
		//		);
		//		
		//		taut("045g", "%table{key a = i; % if j}", EXP);
		//		taut("045h", "%table{key a = i; map val m = n; % if j}", EXP);

	}

	public void test046_table_comprehension() throws Exception {
		//		parsesAs("046a", "%group(key a = i; % for i <- j)", EXP, 
		//				group(list(tabfl(KEY, ioi(a, iv, KEY))),
		//						qcfor(ip, jv)
		//						)
		//		);
		//		
		//		parsesAs("046b", "%group(key a = i; map b = i*i;  % for i <- j)", EXP, 
		//				group(list(tabfl(KEY, ioi(a, iv, KEY)), tabfl_map(VAL, ioi(b, bin(iv, TIMES, iv), VAL) )),
		//						qcfor(ip, jv)
		//						)
		//		);
		//		//String funf = tabfun(fundecl(f, "", k1));
		//		parsesAs("046c", "%group(key a = i; map b = i*i; def f(x)=1;  % for i <- j)", EXP, 
		//				group(list(tabfl(KEY, ioi(a, iv, KEY)), 
		//							   tabfl_map(VAL, ioi(b, bin(iv, TIMES, iv), VAL)),
		//							   (def(f, formals(xp), k1))
		//					     ),
		//						qcfor(ip, jv)
		//						)
		//		);
		//
		//		parsesAs("046d", "%group(key a = i; map b = i*i; def f(x)=1; def s ([x, y...]) = x + s(y); | s ([]) = 1; % for i <- j)", EXP, 
		//				group(list(tabfl(KEY, ioi(a, iv, KEY)), 
		//							   tabfl_map(VAL, ioi(b, bin(iv, TIMES, iv), VAL)),
		//							   (def(f, formals(xp), k1)),
		//							   (def(s, funbody(monobody(s, formals(patlistctor(patlb(xp), patlbe(yp))), bin(xv, PLUS, fc(sv, yv))),
		//									   	monobody(s, formals(patlistctor()), k1))))
		//					     ),
		//						qcfor(ip, jv)
		//						)
		//		);
		//		
		//		parsesAs("046e", "%group{key a = i; % for i <- j}", EXP, 
		//				group(list(tabfl(KEY, ioi(a, iv, KEY))),
		//						qcfor(ip, jv)
		//				)
		//		);
		//		parsesAs("046f", "%group{key a = i; % for i <- j}group", EXP, 
		//				group(list(tabfl(KEY, ioi(a, iv, KEY))),
		//						qcfor(ip, jv)
		//						)
		//		);
		//		doesntParse("046f", "%group{key a = i; %for i <- j}table", EXP, "id equal to group");
		//		
		//		taut("046g", "%group{key a = i; % if j}", EXP);
		//		taut("046h", "%group{key a = i; map val m = n; % if j}", EXP);
		//		
		//		parsesAs("046g", "%group{key prime = prime?(i); val numbers = …i; % for i <- 2 .. 4}", EXP, 
		//			group(list(tabfl(KEY, ioi("prime", fc("<prime?>", iv), KEY)),
		//						tabfl(VAL, ioi("numbers", list4group(iv), VAL))
		//					),
		//					qcfor(ip, bin(k2, DOTDOT, k4))
		//			)
		//		);

	}

	public static String sort(String exp, List<String> ord, String... controls) {
		return QuerySort.detstr(dearr(controls), exp, delist(ord));
	}

	public static String skey(SortOrder order, String keyexp) {
		return SortKey.detstr(order.toString(), keyexp);
	}

	public void test046_query_sort() throws Exception {
		parsesAs("046a", "%sort(a %< b | if c)", EXP, sort(av, list(skey(ASCENDING, bv)), qcif(cv)));
		parsesAs("046b", "%sort(a %> b | if c)", EXP, sort(av, list(skey(DESCENDING, bv)), qcif(cv)));
		parsesAs("046c", "%sort(a %> b %< d | if c)", EXP, sort(av, list(skey(DESCENDING, bv), skey(ASCENDING, dv)),
				qcif(cv)));
		parsesAs("046d", "%sort(a %> b %> d | if c, if d)", EXP, sort(av, list(skey(DESCENDING, bv), skey(DESCENDING,
				dv)), qcif(cv), qcif(dv)));
		parsesAs("046e", "%sort(%sort(a | if 1) %< b | if c)", EXP, sort(sort(av, Collections.EMPTY_LIST, qcif(k1)),
				list(skey(ASCENDING, bv)), qcif(cv)));

	}

	public static String list4group(String exp) {
		return ListForGroup.detstr(exp);
	}

	public void test047_list_for_group() throws Exception {
		parsesAs("047a", "...x", EXP, list4group(xv));
		parsesAs("047a", "...(x+3)", EXP, list4group(parens(bin(xv, PLUS, k3))));
	}

	public static String forr(String label, String pat, String list, String body, boolean inquisitive) {
		return For.detstr(label, pat, list, body, inquisitive);
	}

	public void test048_for() throws Exception {
		parsesAs("048a", "for (i <- i) a;", STMT, forr(no, ip, iv, av, false));
		parsesAs("048a", "for (i <~ i) a;", STMT, forr(no, ip, iv, av, true));
		parsesAs("048a", "i: for (i <- i) a;", STMT, forr(i, ip, iv, av, false));
		parsesAs("048b", "i: for (i && j <- i) a;", STMT, forr(i, patand(ip, jp), iv, av, false));
		parsesAs("048d", "for (i <- i) {a := a;}", STMT, forr(no, ip, iv, seq(assign(assid(a), av)), false));
		parsesAs("048e", "for (i <- i) if (i) i;", STMT, forr(no, ip, iv, IF(iv, iv, no), false));
		parsesAs("048f", "for (i <- i) if (i) for(j<-j) j;", STMT, forr(no, ip, iv, IF(iv, forr(no, jp, jv, jv, false),
				no), false));
		parsesAs("048f", "for (i <- i) if (i) j: for(j<-j) j;", STMT, forr(no, ip, iv, IF(iv,
				forr(j, jp, jv, jv, false), no), false));

		parsesAs("048g", "for (i <- i) {i;}for", STMT, forr(no, ip, iv, seq(iv), false));

		taut("048h", "for (i <- i){i;}", STMT);
		taut("048h", "for (i <~ i){i;}", STMT);
	}

	public void test049_backquoted_ids() throws Exception {
		parsesAs("049a", "``", EXP, "<>");
		parsesAs("049a", "`12`", EXP, "<12>");
		parsesAs("049a", "`ß`", EXP, "<ß>");
		parsesAs("049a", "`ßé•°·`", EXP, "<ßé•°·>");
	}

	public static String stri(QuoteStyle qs, String... bits) {
		return StringWithInterpolations.detstr(dearr(bits), qs);
	}

	public static String stl(String s) {
		return StringBitText.detstr(s);
	}

	public static String stv(String s) {
		return StringBitVar.detstr(s, false);
	}

	public static String stbq(String s) {
		return StringBitVar.detstr("<" + s + ">", true);
	}

	public void test050_long_string() throws Exception {

		String teststring = "\\\\ \\n \\r \\b \\t \\$ \\' \\\" $x $purr $`frogge_thwaite_11` $`!!` ";
		String targstring = "\\ \n \r \b \t $ ' \" $x $purr $(frogge_thwaite_11) ";
		for (QuoteStyle qs : QuoteStyle.values()) {
			String delim = qs.image;
			String subj = delim + teststring + delim;
			taut("050b" + delim, subj, EXP);
			parsesAs("050c" + delim, delim + "" + delim, EXP, "()");
			parsesAs("050d" + delim, delim + "glod" + delim, EXP, "(glod)"); //stri(qs, stl("glod")));
			parsesAs("050e" + delim, delim + "$glod" + delim, EXP, stri(qs, stv("<glod>")));
			parsesAs("050f" + delim, delim + "$g$l" + delim, EXP, stri(qs, stv(gv), stv(lv)));
			parsesAs("050g" + delim, delim + "$g-$l" + delim, EXP, stri(qs, stv(gv), stl("-"), stv(lv)));
			parsesAs("050h" + delim, delim + "$`g`-$l" + delim, EXP, stri(qs, stbq("g"), stl("-"), stv(lv)));
			parsesAs("050i" + delim, delim + "$`~!@`-$l" + delim, EXP, stri(qs, stbq("~!@"), stl("-"), stv(lv)));
			parsesAs("050j" + delim, delim + "$g0 belch" + delim, EXP, stri(qs, stv("<g0>"), stl(" belch")));
			parsesAs("050j" + delim, delim + "$`*` belch$x" + delim, EXP, stri(qs, stbq("*"), stl(" belch"), stv(xv)));
			parsesAs("050k" + delim, delim + "glod $`2`" + delim, EXP, stri(qs, stl("glod "), stbq("2")));
			parsesAs("050l" + delim, delim + "\\nglod\\n" + delim, EXP, "(\nglod\n)");//stri(qs, stl("\nglod\n")));
			parsesAs("050m" + delim, delim + "\\\\" + delim, EXP, "(\\)"); //stri(qs, stl("\\")));
			parsesAs("050n" + delim, delim + " \\\" " + delim, EXP, "( \" )");//stri(qs, stl(" \" ")));
			parsesAs("050o" + delim, delim + " \\\' " + delim, EXP, "( \' )");// stri(qs, stl(" \' ")));
			parsesAs("050p" + delim, delim + " \\n " + delim, EXP, "( \n )");//stri(qs, stl(" \n ")));
			parsesAs("050q" + delim, delim + " \\b " + delim, EXP, "( \b )");//tri(qs, stl(" \b ")));
			parsesAs("050r" + delim, delim + " \\r " + delim, EXP, "( \r )");//stri(qs, stl(" \r ")));
			parsesAs("050s" + delim, delim + " \\t " + delim, EXP, "( \t )");//stri(qs, stl(" \t ")));
			parsesAs("050t" + delim, delim + " \\$ " + delim, EXP, "( $ )");//stri(qs, stl(" $ ")));
			parsesAs("050u" + delim, delim + " \\$" + delim, EXP, "( $)");// stri(qs, stl(" $")));

			doesntParse("050A" + delim, delim + "$" + delim, EXP, "String can\'t end with $");
			doesntParse("050A" + delim, delim + "$x $wozzle $" + delim, EXP, "String can\'t end with $");
			doesntParse("050B" + delim, delim + "\\" + delim, EXP, "");
			doesntParse("050B" + delim, delim + "$x $wozzle \\n \\" + delim, EXP, "");
			doesntParse("050C" + delim, delim + "\\v" + delim, EXP, "");
			doesntParse("050C" + delim, delim + "$`" + delim, EXP, "Unclosed backquote");
			doesntParse("050C" + delim, delim + "$`sporgiu " + delim, EXP, "Unclosed backquote");
			doesntParse("050C" + delim, delim + "$la is my $`sporgiu " + delim, EXP, "Unclosed backquote");
			doesntParse("050D" + delim, delim + "$ " + delim, EXP, "needs an identifier after it");
			doesntParse("050D" + delim, delim + "$(1) " + delim, EXP, "needs an identifier after it");
		}
	}

	public void test051_files() throws Exception {
		fileMustParse("051a", "001-mini-test.th");
		fileMustParse("051a", "aa-mini");
		fileMustParse("051all", "");
	}

	public void test052_spec_examples() throws Exception {
		fileMustParse("052all", "spec-examples");
	}

	public static String var(String var, String init, String typecon) {
		return VarDecl.detstr(var, init, typecon);
	}

	public static String var(String var, String init) {
		return VarDecl.detstr(var, init, tycon());
	}

	public static String tycon1(String t) {
		return TypeConstraint.detstr(t);
	}

	public static String tycon(String... ts) {
		List<String> sts = list();
		for (String t : ts) {
			sts.add(tycon1(t));
		}
		return TypeConstraints.detstr(delist(sts));
	}

	public void test053_var() throws Exception {
		parsesAs("053a", "var a := 1;", STMT, var(a, k1));
		parsesAs("053a", "var a;", STMT, var(a, no));
		taut("053c", "var a;", STMT);
		taut("053c", "var a := 1;", STMT);
	}

	public static final String[] NO_INPUTS = new String[0];

	public static String patdecon(String id, String... subpats) {
		return PatExtract.detstr(qn(id), dearr(subpats));
	}

	public static String patex(String id, String inputs, String... subpats) {
		return PatExtract.detstr(id, dearr(subpats));
	}

	public void test054_pat_decon() throws Exception {
		parsesAs("054a", "c() = 1;", STMT, bind(patdecon(c), k1));
		parsesAs("054a", "c(x) = 1;", STMT, bind(patdecon(c, xp), k1));
		parsesAs("054a", "c(x, +y) = 1;", STMT, bind(patdecon(c, xp, patplus(yp)), k1));
		taut("054d", "Cplx(x, y) = 1", EXP);
		doesntParse("054e", "c(1+x)=1;", STMT, NOTPAT);
	}

	public static String patype(String subpat, String shouldBe) {
		return PatTypeTest.detstr(subpat, shouldBe);
	}

	public void test05A_type() throws Exception {
		parsesAs("05A1", "x:i = 1;", STMT, bind(patype(xp, qn(i)), k1));
		parsesAs("05A2", "x:a.b.c = 1;", STMT, bind(patype(xp, qn(a, b, c)), k1));
		taut("05A3", "x:a.b.c = 1", EXP);
		taut("05A4", "+x && [y, z]:a.b.c = 1", EXP);
	}

	public static String superctor(String qn, String... args) {
		return SuperCtorCall.detstr(qn, dearr(args));
	}

	public void test055_superctor_and_thisctor() throws Exception {
		parsesAs("055a", "class c{new c()={new();}}", CLS, clsdecl(false, c, NO, NO, (ctor(c, formals(),
				seq(superctor(no))))));
		parsesAs("055b", "class c{new c()={new@a();}}", CLS, clsdecl(false, c, NO, NO, ctor(c, formals(),
				seq(superctor(qn(a))))));
		parsesAs("055c", "class c{new c()={new@a.b(1,2);}}", CLS, clsdecl(false, c, NO, NO, ctor(c, formals(),
				seq(superctor(qn(a, b), k1, k2)))));
		parsesAs("055c", "class c{new c()={new@a.b(1,2,);}}", CLS, clsdecl(false, c, NO, NO, ctor(c, formals(),
				seq(superctor(qn(a, b), k1, k2)))));
	}

	public static String assign2sub(String table, String... subscripts) {
		return AssignToSubscripted.detstr(table, dearr(subscripts));
	}

	public void test056_assign_to_subscript() throws Exception {
		parsesAs("056a", "c(c,c):=d;", STMT, assign(assign2sub(cv, cv, cv), dv));
		parsesAs("056a", "c(c):=d;", STMT, assign(assign2sub(cv, cv), dv));
		parsesAs("056a", "c():=d;", STMT, assign(assign2sub(cv), dv));
		parsesAs("056a", "c()():=d;", STMT, assign(assign2sub(fc(cv, "")), dv));
		taut("056e", "{c(1, 2) := 3;}", STMT);
	}

	public static String throow(String exn) {
		return Throw.detstr(exn);
	}

	public void test057_throw() throws Exception {
		parsesAs("057a", "throw 1;", STMT, throow(k1));
		parsesAs("057b", "throw 'a';", STMT, throow("(a)"));
		taut("057c", "throw 1;", STMT);

	}

	public static String spawn(String... bits) {
		return Spawn.detstr(null, dearr(bits));
	}

	public static String sync(String name, String funbody) {
		return SyncDecl.detstr(name, funbody);
	}

	public static String sync(String name, String args, String body) {
		return sync(name, funbody(monobody(name, (args), body)));
	}

	public static String async(String name, String funbody) {
		return AsyncDecl.detstr(name, funbody);
	}

	public static String async(String name, String args, String body) {
		return async(name, funbody(monobody(name, (args), body)));
	}

	public static String procinit(String cmd) {
		return ProcInit.detstr(cmd);
	}

	public static String procbody(String cmd) {
		return ProcBody.detstr(cmd);
	}

	public void test058_spawn() throws Exception {
		parsesAs("058a", "spawn{}", EXP, spawn());
		parsesAs("058a", "spawn{}spawn", EXP, spawn());
		parsesAs("058b1", "spawn{sync a()=1;}", EXP, spawn(sync(a, formals(), k1)));
		parsesAs("058b2", "spawn{sync a(){a();b();}}", EXP, spawn(sync(a, formals(), seq(fc(av, ""), fc(bv, "")))));
		parsesAs("058b3", "spawn{async a()=1;}", EXP, spawn(async(a, formals(), k1)));
		parsesAs("058b4", "spawn{async a(){a();b();}}", EXP, spawn(async(a, formals(), seq(fc(av, ""), fc(bv, "")))));
		parsesAs("058c", "spawn{val a = 1; var b := 2; fun m(x) = 3;}", EXP, spawn(membind(ap, k1), memvar(b, k2),
				fundecl(m, xp, k3)));
		parsesAs("058d", "spawn{initially {1;2;}}", EXP, spawn(procinit(seq(k1, k2))));
		parsesAs("058d2", "spawn{body {1;2;}}", EXP, spawn(procbody(seq(k1, k2))));
		taut("058e", "spawn{sync a() = {1;} async a() = {2;} a = 3; var a := 4; fun a(x) = {5;}}", EXP);
		taut("058f", "spawn f(1);", STMT);
		taut("058g", "spawn f(1, 2);", STMT);
		parses("058f", "spawn f(1,);", STMT);
		parses("058g", "spawn f(1, 2,);", STMT);
	}

	public static String signalstmt(String recvr, String id, String... args) {
		return AsyncStmt.detstr(recvr, id, dearr(args),null);
		
	}
	public static String signalstmt2(String recvr, String id, String security, String... args) {
		return AsyncStmt.detstr(recvr, id, dearr(args),security);

	}

	public static String rpc(String recvr, String id, String timeout, String... args) {
		return SyncStmt.detstr(recvr, id, dearr(args), timeout, null, null);
	}
	
	public static String rpc2(String recvr, String id, String timeout, String security, String... args) {
		return SyncStmt.detstr(recvr, id, dearr(args), timeout, null, security);
	}
	

	public static String send(String recvr, String msg) {
		return Send.detstr(recvr, msg, null);
	}
	public static String send(String recvr, String msg, String tokens) {
		return Send.detstr(recvr, msg, tokens);
	}

	public void test059_sends() throws Exception {
		parsesAs("059a", "a <-- b();", STMT, signalstmt(av, b, ""));
		parsesAs("059a", "a <-- b() security a;", STMT, signalstmt2(av, b, av, ""));
		parsesAs("059a", "a+2 <-- b(1+2);", STMT, signalstmt(bin(av, PLUS, k2), b, bin(k1, PLUS, k2)));
		taut("059b", "{a <-- b();}", STMT);
		parsesAs("059c", "a <-> b(c);", STMT, rpc(av, b, no, cv));
		parsesAs("059c", "a <-> b(c) security a;", STMT, rpc2(av, b, no, av, cv));
		parsesAs("059d", "x = a <->b(c);", STMT, bind(xp, rpc(av, b, no, cv)));
		parsesAs("059e", "a <-> b(c) timeout(1);", STMT, rpc(av, b, k1, cv));
		parsesAs("059f", "x = a <->b(c) timeout(x);", STMT, bind(xp, rpc(av, b, xv, cv)));
		taut("059g", "{x = a<->b(c) timeout(x);}", STMT);

		parsesAs("059h", "x <<< 1;", STMT, send(xv, k1));
		parsesAs("059h", "x <<< 1 security 2;", STMT, send(xv, k1, k2));
		parsesAs("059h", "x <<< f(1);", STMT, send(xv, fc(fv, k1)));
		parsesAs("059h", "x <<< 1 && 2;", STMT, send(xv, varop(AND, k1, k2)));
		taut("059i", "{x <<< 1&&2;}", STMT);
		parses("059j", "x <-> s(,);", STMT);
		parses("059j", "x <-> s(1,);", STMT);
		parses("059j", "x <-> s(1,2,);", STMT);
		parses("059j", "x <-> s(1,2,) timeout(true){};", STMT);
	}

	public static String recv(String timeoutLen, String timeoutCmd, String... cases) {
		return Recv.detstr(dearr(cases), timeoutLen, timeoutCmd);
	}

	public void test060_recv() throws Exception {
		parsesAs("060a", "recv { [] = {1;} }", STMT, recv(no, no, cas(patlistctor(), seq(k1))));
		parsesAs("060a", "recv { [] checked = {1;} }", STMT, recv(no, no, caschecked(patlistctor(), seq(k1))));
		parsesAs("060b", "recv { [] = {1;} timeout (2)  {3;} }", STMT, recv(k2, seq(k3), cas(patlistctor(), seq(k1))));
		taut("060c", "recv{[] = {1;} timeout(2){3;}}", STMT);
		parses("060d", "{e = (recv{ x from $(kim) => x});}", STMT);
		taut("060e", "recv{a from b envelope c prio 1 = {2;}}", STMT);
		parses("060f", "recv{x => x}", STMT);
	}

	public static String serveblock(String name, String formals, String body) {
		return ServeBlock.detstr(name, formals, body);
	}

	public static String serve(String before, String after, String timeout, String timeoutCmd) {
		return Serve.detstr(before, after, timeout, timeoutCmd, delist(NO));
	}
	public static String serve(String before, String after, String timeout, String timeoutCmd, String catches) {
		return Serve.detstr(before, after, timeout, timeoutCmd, catches);
	}

	public void test061_serve() throws Exception {
		parsesAs("061a", "serve", STMT, serve(no, no, no, no));
		parsesAs("061a", "serve;", STMT, serve(no, no, no, no));

		String a_before = serveblock("before", "[]", seq(av));
		String b_after = serveblock("after", "[]", seq(bv));
		parsesAs("061b1", "serve before{a;}before", STMT, serve(a_before, no, no, no));
		parsesAs("061b2", "serve before{a;}before after{b;}", STMT, serve(a_before, b_after, no, no));
		parsesAs("061b3", "serve after{b;} before{a;}before ", STMT, serve(a_before, b_after, no, no));

		parsesAs("061c1", "serve timeout(a){}", STMT, serve(no, no, av, seq()));
		doesntParse("061c2", "serve before{} before{}", STMT, "two before clauses");
		doesntParse("061c3", "serve after{} after{}", STMT, "two after clauses");
		doesntParse("061c4", "serve after{} before{} after{}", STMT, "two after clauses");
		taut("016d", "serve before{a;} after{b;} timeout(c){};", STMT);
		taut("016d", "serve before(x){a;} after(x, y){b;} timeout(c){};", STMT);
	}

	public static String ret(String exp) {
		return fisher.syn.Return.detstr(exp);
	}

	public void test062_return() throws Exception {
		parsesAs("062a", "return;", STMT, ret(no));
		parsesAs("062b", "return 1;", STMT, ret(k1));
	}

	public static String prf(String id, String pat) {
		return PatRecordField.detstr(id, pat);
	}

	public static String patrec(String... prfs) {
		return PatRecordCtor.detstr(dearr(prfs));
	}

	public static String NOTPAT2 = "Can't convert this expression to pattern";

	public void test063_record_patterns() throws Exception {
		parsesAs("063a", " x ~ ‹ x:$(1) ›", EXP, tilde(xv, patrec(prf(x, patinterp(k1)))));
		parsesAs("063b", "‹›", PAT, patrec());
		parsesAs("063c", "‹ a:‹› ›", PAT, patrec(prf(a, patrec())));
		parsesAs("063d", "‹a:x, b:_›", PAT, patrec(prf(a, xp), prf(b, patwild())));
		doesntParse("063f", "‹a:x+1›", PAT, NOTPAT2);
		doesntParse("063g", "[_, x, C(a, ‹a: d && [$(x+1), (it>3)?, t+1]›)]", PAT, NOTPAT2);
		doesntParse("063g", "[_, x, C(a, ‹a: d || [$(x+1), (it>3)?, t+1]›)]", PAT, NOTPAT2);
	}

	public static String brackall(String fun, String... args) {
		return BracketCall.detstr(fun, dearr(args));
	}

	public static String reckall(String fun, String... rargs) {
		return RecordCall.detstr(fun, dearr(rargs));
	}

	public void test064_bracket_and_record_calls() throws Exception {
		parsesAs("064a", "f[]", EXP, brackall(fv));
//		parsesAs("064b", "f{: :}", EXP, reckall(fv));
		parsesAs("064c", "f[1]", EXP, brackall(fv, k1));
		parsesAs("064d", "f[1, x+2]", EXP, brackall(fv, k1, bin(xv, PLUS, k2)));

//		parsesAs("064e", "f‹a:1›", EXP, reckall(fv, rf(a, k1)));
//		parsesAs("064e", "f‹a:1, b:x+2›", EXP, reckall(fv, rf(a, k1), rf(b, bin(xv, PLUS, k2))));
	}

	public static String opab(String target, OpAB opab, String amount) {
		return OpABExp.detstr(target, opab.toString(), amount);
	}

	public void test065_opab() throws Exception {
		parsesAs("065a", "a += 1;", STMT, opab(assid(a), PLUSAB, k1));
		parsesAs("065b", "a -= 1;", STMT, opab(assid(a), MINUSAB, k1));
		parsesAs("065c", "a *= 1;", STMT, opab(assid(a), TIMESAB, k1));
		parsesAs("065d", "a /= 1;", STMT, opab(assid(a), FDIVAB, k1));
		parsesAs("065e", "a(1) /= 1;", STMT, opab(assign2sub(av, k1), FDIVAB, k1));
		parsesAs("065f", "a.b /= fn(x)=x;", STMT, opab(assfr(av, b), FDIVAB, anonfun(formals(xp), xv)));
	}

	public void test066_cons_append_addto() throws Exception {
		parsesAs("066a", "a @ b", EXP, bin(av, APPEND, bv));
		parsesAs("066a", "a :: b", EXP, bin(av, CONS, bv));
		parsesAs("066a", "a @= b", EXP, bin(av, ADDTO, bv));
		parsesAs("066a", "a \\= b", EXP, bin(av, DELFROM, bv));
		parsesAs("066b", "x + 1 :: b", EXP, bin(bin(xv, PLUS, k1), CONS, bv));
		parsesAs("066c", "a @ b @ c", EXP, bin(bin(av, APPEND, bv), APPEND, cv));
	}

	public static String patrf(String id, String pat) {
		return PatRecordField.detstr(id, pat);
	}

	public void test067_auto_field_in_record() throws Exception {
		parsesAs("067a", "‹a›", EXP, rec(rf(a, av)));
		parsesAs("067b", "{: b :}", PAT, patrec(patrf(b, bp)));
		parsesAs("067b", "{: b : _ :}", PAT, patrec(patrf(b, patwild())));
		parsesAs("067b", "{: b : _, c: [] :}", PAT, patrec(patrf(b, patwild()), patrf(c, patlistctor())));
		parsesAs("067b", "{: b : _, c: [] , d:}", PAT, patrec(patrf(b, patwild()), patrf(c, patlistctor()),
				patrf(d, dp)));
		parsesAs("067g", "‹a,b,c›", EXP, rec(rf(a, av), rf(b, bv), rf(c, cv)));
		parsesAs("067g", "‹a,b:1,c›", EXP, rec(rf(a, av), rf(b, k1), rf(c, cv)));
	}

	public static String valof(String... stmts) {
		return Valof.detstr(dearr(stmts), false);
	}

	public void test068_valof() throws Exception {
		parsesAs("068a", "valof{}", EXP, valof());
		parsesAs("068b", "valof{x <<< 1;}", EXP, valof(send(xv, k1)));
		parsesAs("068b", "valof{x <<< 1; class c{}}", EXP, valof(send(xv, k1), clsdecl(false, c, NO, NO)));
	}

	public void test069_semicolons_at_the_end() throws Exception {
		parses("069a", "do 1; while(2);", STMT);
		parses("069b", "do 1; while(2);;;;", STMT);
		parses("069c", "class C{};", STMT);
		parses("069d", "class C{};;;;", STMT);
		parses("069e", "try {1;} catch{_ = 1};", STMT);
	}

	public void test070_user_def_ops() throws Exception {
		parsesAs("070a", "@!1", EXP, mc(k1, "@!", none));
		parsesAs("070a", "@~1", EXP, mc(k1, "@~", none));
		parsesAs("070a", "@$1", EXP, mc(k1, "@$", none));
		parsesAs("070a", "@!@$1", EXP, mc(mc(k1, "@$", none), "@!", none));

		parsesAs("070d", "1 @+ 2", EXP, mc(k1, "@+", k2));
		parsesAs("070d", "1 @- 2", EXP, mc(k1, "@-", k2));
		parsesAs("070d", "1 @* 2", EXP, mc(k1, "@*", k2));
		parsesAs("070d", "1 @/ 2", EXP, mc(k1, "@/", k2));

		doesntParse("070h", "1 @+ 2 @+ 3", EXP, "Encountered");

		parsesAs("070i", "1 <@ 2 @> 3", EXP, mc(k1, "<@@>", comma(k2, k3)));
		parsesAs("070i", "1 [@ 2 @] 3", EXP, mc(k1, "[@@]", comma(k2, k3)));
	}

	public void test071_exp_doom() throws Exception {
		parsesAs("071a", "!a(b)", EXP, una("!", fc(av, comma(bv))));
		parsesAs("071b", "a::b", EXP, bin(av, CONS, bv));
		parsesAs("071c", "a::b::c", EXP, bin(av, CONS, bin(bv, CONS, cv)));
		parsesAs("071c", "a::b::c::d", EXP, bin(av, CONS, bin(bv, CONS, bin(cv, CONS, dv))));

	}

	public void test072_optional_semi_in_group() throws Exception {
		parses("072a1", "%group(k=a){c=%count | for a <- 1}", EXP);
		parses("072a2", "%group(k=a){c=%count d = %rev 2 e = %first 1 %then 2 %after 3 f=%count | for a <- 1}", EXP);
		taut("072a", "%group(k=a){val c = %first 1 %then 1+c; | for a <- 1..4}", EXP);
	}

	private static void purityTest(String loc, String code, SyntacticClass cls, boolean ispure) throws Exception {
		Puretic pt = (Puretic) parses(loc, code, cls);
		assertEquals(loc + "/pure", pt.isMarkedPure(), ispure);
	}

	public void test073_purity() throws Exception {
		try{
			TestUtils.NO_CLASS_DESUGARING_SO_I_CAN_DO_PARSER_TESTS = true;
		
		purityTest("073a", "class A : pure {}", CLS, true);
		purityTest("073a", "class A : pure extends B {}", CLS, true);
		purityTest("073b", "class A {}", CLS, false);
		taut("073c", "class A:pure{}", CLS);
		taut("073c", "class A:pure extends B(){}", CLS);
		purityTest("073d", "object : pure {}", EXP, true);
		purityTest("073d", "object : pure extends B{}", EXP, true);
		purityTest("073e", "object  {}", EXP, false);
		taut("073f", "object:pure {}", EXP);
		taut("073f'", "object:pure extends B(){}", EXP);
		}
		finally {
			TestUtils.NO_CLASS_DESUGARING_SO_I_CAN_DO_PARSER_TESTS = false;
		}
	}

	public void test074_trailing_semicolons() throws Exception {
		parses("074a", "class Fuddurbud{};", STMT);
		parses("074b", "while(true) {reblabber();};", STMT);
		parses("074c", "while(true) while(true) {reblobbler();};", STMT);
//		parses("074c","fun f(){1;};", STMT);
	}
	
	public void test075_badpat() throws Exception {
		parses("075a", "{x={:a:1, b:2:}; println(x ~ {: a:int :});}", STMT);
	}
	
	public void test074_empty_string_pat() throws Exception {
		parses("074a", "''", PAT);
	}

	public void test800_parsing_modules() throws Exception {
		fileMustParse("800", "module-examples");
	}

	public void test801_oopsla_09() throws Exception {
		fileMustParse("801", "oopsla09");
	}

	public void test802_misc() throws Exception {
		fileMustParse("802", "misc");
	}

}
