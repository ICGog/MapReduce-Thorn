
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

import static fisher.ingest.Ingester.ASSIGNMENT_LIST_MISMATCH;
import static fisher.ingest.Ingester.A_CONSTRUCTOR_MUST_HAVE_THE_SAME_NAME_AS_THE_CLASS_IT_CONSTRUCTS;
import static fisher.ingest.Ingester.A_SPAWN_CAN_HAVE_ONLY_ONE_BODY;
import static fisher.ingest.Ingester.A_SPAWN_CAN_HAVE_ONLY_ONE_INIT;
import static fisher.ingest.Ingester.A_SPAWN_MUST_HAVE_A_BODY_CLAUSE;
import static fisher.ingest.Ingester.CLSDECL_VAL_MATCH_MUST_HAVE_SUBJECT;
import static fisher.ingest.Ingester.FROM_IN_MATCH;
import static fisher.ingest.Ingester.MATCH_PRIO_IN_ORDER;
import static fisher.ingest.Ingester.ONLY_ONE_MAP;
import static fisher.ingest.Ingester.PRE_ELLIPSIS_ONLY_IN_GROUP;
import static fisher.ingest.Ingester.TABLE_FIELDS_CANNOT_HAVE_INITIALIZERS;
import static fisher.ingest.Ingester.TABLE_QUERY_FIELDS_NEED_INITIALIZERS;
import static fisher.ingest.Ingester.THIS_IS_A_PATTERN_BUT_NOT_AN_EXPRESSION;
import static fisher.ingest.Ingester.WRONG_NUMBER_OF_ARGS;
import static fisher.parser.SyntacticClass.CLS;
import static fisher.parser.SyntacticClass.EXP;
import static fisher.parser.SyntacticClass.STMT;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import junit.framework.TestCase;
import fisher.ingest.Ingester;
import fisher.parser.SyntacticClass;
import fisher.syn.Cmd;
import fisher.syn.OpExp;
import fisher.syn.core.Syntax;
import fisher.util.Bard;
import fisher.util.Compilation;
import fisher.util.CompilerMessage;


public  class  IngestionTest extends TestCase  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	
	/**
	 * Check that the parse tree of <code>subject</code> has precisely one warning attached to it
	 * after check1.  A primitive little test, just to get going.
	 * @param loc
	 * @param subject
	 * @param cls
	 * @throws Exception
	 */
	private void ingestionWarningCount(String loc, String subject, SyntacticClass cls, int count) throws Exception {
		Compilation.current.reset();
		String loca = loc + "/ingestionWarningCount";
		Syntax syn = TestUtils.parse(loca, subject, cls);
		Ingester.check(syn);
		List<CompilerMessage> messages = Compilation.current.messages;
		assertEquals(loca + "/count", count, messages.size() );
	}
	
	public static class Clue {
		public int count;
		public String regexp;
		public Clue(int count, String regexp) {
			super();
			this.count = count;
			this.regexp = regexp;
		}
		public String toString() {
			return count + " matches for '" + regexp + "'";
		}		
	}
	
	public static Clue clue(int count, String regexp) {
		return new Clue(count, regexp);
	}
	
	public static void ingestTest(String loc, String subject, SyntacticClass cls, Clue... clues) throws Exception {
		Compilation.current.reset();
		String loca = loc + "/ingestionWarningCount";
		Syntax syn = TestUtils.parse(loca, subject, cls);
		coreIngestTest(loc, syn, clues);
		
	}

	private static void coreIngestTest(String loc, Syntax syn, Clue... clues) throws Exception {
		Ingester.check(syn);
		List<CompilerMessage> messages = Compilation.current.messages;
		Set<CompilerMessage> unmatched = new HashSet<CompilerMessage>();
		unmatched.addAll(messages);
		for (Clue clue : clues) {
			int nMatching = 0;
			List<CompilerMessage> matchesThis = new ArrayList<CompilerMessage>();
			List<CompilerMessage> doesntMatchThis = new ArrayList<CompilerMessage>();
			for (CompilerMessage message : messages) {
				if (message.matchesClue(clue.regexp)) {
					nMatching ++;
					unmatched.remove(message);
					matchesThis.add(message);
				}
				else {
					doesntMatchThis.add(message);
				}
			}
			if (nMatching != clue.count) {
				String yowl = 
					loc + "/ingest[" + clue + "] -- wrong number of matches.\n"
					+ "Expected " + clue.count + " but got "+ nMatching + ".\n"
					+ "They are: \n"
					+ Bard.sep(matchesThis, "\n")
					+ "\n========================================================================\n"
					+ "The non-matching messages are: \n"
					+ Bard.sep(doesntMatchThis, "\n")					
					;
				assertTrue(yowl, false);
			}
		}
		if (! unmatched.isEmpty()) {
			String yowl = loc + "/ingest/unexpected-errors\n"
			+ "All messages should have been matched, but that is not what occurred: \n"
			+ "The unmatched ones are: \n"
			+ Bard.sep(unmatched, "\n")					
			;
		assertTrue(yowl, false);
		}
	}
	
	public void test001_getting_going() throws Exception {
		ingestionWarningCount("001", "a+b", EXP, 0);
	}
	
	public void test002_ctor_name() throws Exception {
		ingestionWarningCount("002a", "class C{ new C(){} }", CLS, 0); 
		ingestionWarningCount("002a", "class C{ new NOPE(){} }", CLS, 1); 
		ingestTest("002a", "class C{new NOPE(){}}", CLS, clue(1, A_CONSTRUCTOR_MUST_HAVE_THE_SAME_NAME_AS_THE_CLASS_IT_CONSTRUCTS));
	}
	
	public void test003_assign() throws Exception {
		ingestionWarningCount("003a", "a := b;", STMT, 0);
		ingestionWarningCount("003a", "a,b := b;", STMT, 1);
		ingestionWarningCount("003a", "a,b := b,a;", STMT, 0);
		ingestTest("003b", "{a := b,c; a,b := b;}", STMT, clue(2, ASSIGNMENT_LIST_MISMATCH));
	}
	
	public void test004_pats_which_are_not_exps() throws Exception {
		ingestTest("004a", "[_, $(1), (1 == 1)?, 1+$(1), (1+2*(4/_))]", EXP, clue(5, THIS_IS_A_PATTERN_BUT_NOT_AN_EXPRESSION));
	}
	
	public void test005_fixity_error() throws Exception {
		// Fixity errors are internal, and there's no way to construct them except synthetically.
		// (Or so we hope anyway).
		Compilation.current.reset();
		String loca = "005a";
		Syntax syn = TestUtils.parse(loca, "-x", EXP);
		OpExp e = (OpExp) syn;
		Cmd extra = (Cmd) TestUtils.parse(loca, "1", EXP);
		e.operands.add(extra);
		coreIngestTest(loca, e, clue(1, WRONG_NUMBER_OF_ARGS));
	}
	
	public void test006_map_fields() throws Exception {
		ingestTest("006a", "table(k){map a; map b;}", EXP, clue(1, ONLY_ONE_MAP));
		ingestTest("006b", "table(k){map a, b;}", EXP, clue(1, ONLY_ONE_MAP));
		ingestTest("006b", "table(k){map a, b; map var c;}", EXP, clue(2, ONLY_ONE_MAP));
		
	}
	
	public void test007_table_fields_have_init_but_table_query_fields_dont() throws Exception {
		ingestTest("007a", "table(k){val a = 1;}", EXP, clue(1, TABLE_FIELDS_CANNOT_HAVE_INITIALIZERS));
		ingestTest("007a", "table(k){val a;}", EXP, clue(0, TABLE_FIELDS_CANNOT_HAVE_INITIALIZERS));
		ingestTest("007b1", "%table(b=1){val a; | if b}", EXP, clue(1, TABLE_QUERY_FIELDS_NEED_INITIALIZERS));
		ingestTest("007b2", "%table(b=1){val a ; | if b}", EXP, clue(1, TABLE_QUERY_FIELDS_NEED_INITIALIZERS));
		ingestTest("007c3", "table(k){val a = %table(d=1){val a; | if b};}", EXP, 
				clue(1, TABLE_FIELDS_CANNOT_HAVE_INITIALIZERS),
				clue(1, TABLE_QUERY_FIELDS_NEED_INITIALIZERS)
		);
		ingestTest("007c2", "%table(k=1){val a = table(k){val a;}; | if b}", EXP, 
				clue(0, TABLE_FIELDS_CANNOT_HAVE_INITIALIZERS),
				clue(0, TABLE_QUERY_FIELDS_NEED_INITIALIZERS)
		);

		ingestTest("007j", "%table(x){val a = _; | if b}", EXP, 
				clue(1, TABLE_QUERY_FIELDS_NEED_INITIALIZERS),
				clue(4, THIS_IS_A_PATTERN_BUT_NOT_AN_EXPRESSION));

	}
	
	public void test008_no_prio_in_match() throws Exception {
		ingestTest("008a", "match(1){_ from 1 = {2;}}", STMT, clue(1, FROM_IN_MATCH));
		ingestTest("008b", "match(1){_ prio 0 => {} | _ prio 1 => {}}", STMT, clue(1, MATCH_PRIO_IN_ORDER));
	}
	
	public void test009_uninit_nonvar_pat_in_class() throws Exception {
		ingestTest("009a", "class C { val [a,b]; }", STMT, clue(1, CLSDECL_VAL_MATCH_MUST_HAVE_SUBJECT));
		ingestTest("009a", "class C { val [a,b] = 0; }", STMT, clue(0, CLSDECL_VAL_MATCH_MUST_HAVE_SUBJECT));
	}
	
	public void test010_spawn() throws Exception {
		ingestTest("010a", "spawn{}", EXP, clue(1, A_SPAWN_MUST_HAVE_A_BODY_CLAUSE));
		ingestTest("010b", "spawn{ body{} body{} }", EXP, clue(1, A_SPAWN_CAN_HAVE_ONLY_ONE_BODY));
		ingestTest("010c", "spawn{ body{} initially{} initially{} }", EXP, clue(1, A_SPAWN_CAN_HAVE_ONLY_ONE_INIT));
		ingestTest("010d", "spawn{ a = 1; var b := 2; body{} }", EXP);
		
	}
	
	public void test011_javaly_fun() throws Exception {
		ingestTest("011a", "module M{javaly fun f() = fisher.runtime.builtInFun.SomeFunsForTestingJavalyFun.zero ; }", SyntacticClass.MODU);
		ingestTest("011a", "module M{javaly fun f(x) = fisher.runtime.builtInFun.SomeFunsForTestingJavalyFun.one ; }", SyntacticClass.MODU);
	}
	
	public void test012_javaly_class() throws Exception {
		ingestTest("012a", "module M{javaly class C = fisher.test.classes.Whatnot { def zero() = dim; def one(y) = un; new C(); new C(x,y);}}", SyntacticClass.MODU);
	}
	
	public void KNOWN_FAILURE_test008_listforgroup_only_works_in_a_group() throws Exception {
		// TODO Doing this will require more than 15 minutes work, so I can't do it today.
		ingestTest("008a", "...1", EXP, clue(1, PRE_ELLIPSIS_ONLY_IN_GROUP));
		ingestTest("008a", "%group(key k = ...1; | if 1)", EXP, clue(1, PRE_ELLIPSIS_ONLY_IN_GROUP));
	}
	
	
	
}
