
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.desugar;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.swing.tree.ExpandVetoException;

import fisher.ingest.Ingester;
import fisher.parser.Token;
import fisher.runtime.BoolTh;
import fisher.runtime.ListTh;
import fisher.syn.*;
import fisher.syn.core.ColSpecial;
import fisher.syn.core.Id;
import fisher.syn.core.Op;
import fisher.syn.core.SortOrder;
import fisher.syn.interfaces.TableMember;
import fisher.syn.visitor.VisitQueryAbstract;
import fisher.syn.visitor.VisitQueryControl;
import fisher.util.Bard;
import fisher.util.DangerLevel;
import fisher.util.Doom;
import fisher.util.FisherException;
import static fisher.util.Bard.*;
import static fisher.syn.core.Op.*;

public  class  QueryDesugarer extends AbstractDesugarer implements VisitQueryControl<Cmd, Cmd, RuntimeException>  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	private List<Cmd> extraStuffForPrelude = new ArrayList<Cmd>();
	private Id first;
	private Id result;
	private Id loopLabel;

	private QueryDesugarer(Token start, Token end) {
		super(start, end);
	}

	public static QuerySort sort(Token start, Token end, List<QueryControl> controls, Cmd exp, List<SortKey> keys) {
		QueryDesugarer qd = new QueryDesugarer(start, end);
		Cmd code = qd.sortDesugaring(controls, exp, keys);
		return new QuerySort(start, end, controls, code, exp, keys);
	}

	public static QueryTable table(Token start, Token end, List<QueryControl> controls, List<IdWithOptInit> keys,
			List<TableFields> items) {
		// 
		QueryDesugarer qd = new QueryDesugarer(start, end);
		Cmd code = qd.tableDesugaring(controls, keys, items);
		return new QueryTable(start, end, controls, code, keys, items);
	}

	public static QueryGroup group(Token start, Token end, List<QueryControl> controls, List<QGKey> keys,
			List<QGAccum> accums) {
		QueryDesugarer qd = new QueryDesugarer(start, end);
		Cmd code = qd.groupDesugaring(controls, keys, accums);
		return new QueryGroup(start, end, controls, code, keys, accums);
	}

	public static QueryListComprehension listComprehension(Token start, Token end, List<QueryControl> controls,
			Cmd exp, boolean isAppended) {
		QueryDesugarer qd = new QueryDesugarer(start, end);
		Cmd code = qd.listDesugaring(controls, exp, isAppended);
		return new QueryListComprehension(start, end, controls, code, exp, isAppended);
	}

	public static QuerySwiss swiss(Token start, Token end, List<QueryControl> controls, Cmd exp, Cmd more, Cmd none) {
		// TODO do me!
		return new QuerySwiss(start, end, controls, null, exp, more, none);
	}

	public static QueryFirst first(Token start, Token end, List<QueryControl> controls, Cmd exp, Cmd none, boolean isExp) {
		QueryDesugarer qd = new QueryDesugarer(start, end);
		Cmd code = qd.firstDesugaring(controls, exp, none);
		return new QueryFirst(start, end, controls, code, exp, none, isExp);
	}

	public static QueryAfter after(Token start, Token end, List<QueryControl> controls, Cmd exp) {
		QueryDesugarer qd = new QueryDesugarer(start, end);
		Cmd code = qd.afterDesugaring(controls, exp);
		return new QueryAfter(start, end, controls, code, exp);
	}

	public static QueryQuantifierEvery every(Token start, Token end, List<QueryControl> controls, Cmd pred) {
		QueryDesugarer qd = new QueryDesugarer(start, end);
		Cmd code = qd.boolQuantifierDesugaring(controls, pred, false);
		return new QueryQuantifierEvery(start, end, controls, code, pred);
	}

	public static QueryQuantifierCount count(Token start, Token end, List<QueryControl> controls, Cmd pred) {
		QueryDesugarer qd = new QueryDesugarer(start, end);
		Cmd code = qd.countDesugaring(controls, pred);
		return new QueryQuantifierCount(start, end, controls, code, pred);
	}

	public static QueryQuantifierSome some(Token start, Token end, List<QueryControl> controls, Cmd pred) {
		QueryDesugarer qd = new QueryDesugarer(start, end);
		Cmd code = qd.boolQuantifierDesugaring(controls, pred, true);
		QueryQuantifierSome some = new QueryQuantifierSome(start, end, controls, code, pred);
		return some;
	}
	
	public static QGAccum qgaccumList(Token start, Token end, fisher.syn.core.ColAccess colAccess, ColSpecial special, Id id, Cmd exp, int code) {
		QueryDesugarer qd = new QueryDesugarer(start, end);
		return qd.doqgaccumlist(colAccess, special, id, exp, code);
	}
	public  QGAccum doqgaccumlist(fisher.syn.core.ColAccess colAccess, ColSpecial special, Id id, Cmd exp, int code) {

		switch(code){
		case 0: // %count
			return new QGAccum(start, end, colAccess, special, id, lit(1), op(Op.PLUS, lit(1), varexp(id)), null);
		case 1: // %list
		case -1: // %rev
			Cmd listexp = new ListCtor(start, end, list((ListBit)new ListBitExp(start, end, cmd(exp))));
			Cmd consexpid = op(Op.CONS, cmd(exp), varexp(id));
			Cmd ifonerev = 
				code == -1 ? null : meth(varexp(id), "reversed"); 
					;
			return new QGAccum(start, end, colAccess, special, id, listexp, consexpid, ifonerev);
		default: 
			Doom.internalCatastrophe("Stupid code not understood in dogqaccumlist", null, code);
			return null;
		}
	}

	private Cmd sortDesugaring(List<QueryControl> controls, Cmd exp, List<SortKey> keys) {
		// %sort( E %> K1 %< K2 % C)
		// -->
		// {
		//   val ¬©ascendings¬© = [false, true];
		//   var ¬©sortables@ := [];
		//   do {
		//     ~~ C ~~
		//       ¬©sortables¬© := internal_sortable(E, ¬©ascendings¬©, K1, K2) :: ¬©sortables¬©;
		//   } until(true);
		//   internal_sort(¬©sortables¬©);
		produceFirstIfNeeded(controls, false);

		Id ascendings = DesugarUtils.gensym(start, "ascendings");
		ListTh ascVals = ListTh.EMPTY;
		for (SortKey sk : keys) {
			ascVals = ascVals.cons(BoolTh.of(sk.sortOrder == SortOrder.DESCENDING));
		}
		Bind bindAscending = bind(ascendings, lit(ascVals.reversed()));
		extraStuffForPrelude.add(bindAscending);
		Id sortables = DesugarUtils.gensym(start, "sortables");
		VarDecl varSortables = vardecl(sortables, lit(ListTh.EMPTY));
		extraStuffForPrelude.add(varSortables);

		// loop body:
		List<Cmd> operands = new ArrayList<Cmd>();
		operands.add((Cmd) exp.ingestedDeepCopy(start, end)); // payload
		operands.add((Cmd) varexp((Id) ascendings.ingestedDeepCopy(start, end))); // ascendings
		for (SortKey sk : keys) {
			operands.add((Cmd) sk.key.ingestedDeepCopy(start, end));
		}
		Cmd internal_sortable = new OpExp(start, end, Op.INTERNAL_SORTABLE, operands);
		Cmd assign_int_sortable = assign(sortables, op(Op.CONS, internal_sortable, varexp(sortables)));
		Cmd body = basic_outer_body(assign_int_sortable);
		Cmd queryLoop = controlLoop(loopLabel, controls, body);

		// postlude
		Cmd returnResult = op(Op.INTERNAL_SORT, varexp(sortables));

		Cmd ult = buildUltimateQuery(null, queryLoop, returnResult);
		//		System.out.println("sortDesugaring's " + ult.toString());
		return ult;

	}

	private Cmd listDesugaring(List<QueryControl> controls, Cmd exp, boolean isAppended) {
		if (isAppended) {
			return listDesugaringAppended(controls, exp);
		} else {
			return listDesugaringConsed(controls, exp);
		}
	}

	private Cmd listDesugaringAppended(List<QueryControl> controls, Cmd exp) {
		// %[E... % C1] 
		// -->
		// {
		//   var ¬©result¬© := [];
		//   @query@: do {
		//      ~~C1~~
		//        @result@ := ¬©result¬© @ E;
		//   } until(true);
		//   ¬©result¬©
		produceFirstIfNeeded(controls, false);
		Literal initialResult = lit(ListTh.EMPTY);
		VarDecl resultDecl = produce_result_and_loopLabel(initialResult);

		Cmd newResult = op(APPEND, varexp(result), exp);
		Cmd body = basic_outer_body(assign(result, newResult));
		Cmd queryLoop = controlLoop(loopLabel, controls, body);
		Cmd returnResult = varexp(result);
		return buildUltimateQuery(resultDecl, queryLoop, returnResult);
	}

	private Cmd listDesugaringConsed(List<QueryControl> controls, Cmd exp) {
		// %[E % C1] 
		// -->
		// {
		//   var ¬©result¬© := [];
		//   @query@: do {
		//      ~~C1~~
		//        @result@ := E :: ¬©result¬© ;
		//   } until(true);
		//   ~!@reverse(¬©result¬©);
		produceFirstIfNeeded(controls, false);
		Literal initialResult = lit(ListTh.EMPTY);
		VarDecl resultDecl = produce_result_and_loopLabel(initialResult);

		Cmd newResult = op(CONS, exp, varexp(result));
		Cmd body = basic_outer_body(assign(result, newResult));
		Cmd queryLoop = controlLoop(loopLabel, controls, body);
		Cmd returnResult = meth(varexp(result), "reversed"); // op(Op.INTERNAL_REVERSE, varexp(result)); 
		return buildUltimateQuery(resultDecl, queryLoop, returnResult);
	}

	public Cmd boolQuantifierDesugaring(List<QueryControl> controls, Cmd pred, boolean isSome) {
		/*
		 * Input: %some ( B % C1, C2, C3 )
		 * 
		 * Output: { var ¬©result¬© := false; ¬©query¬©: do { ~~C1~~ ~~C2~~ ~~C3~~ if
		 * (B) { ¬©result¬© := true; break ¬©query¬©; } } until(false); result; } //
		 * The outer loop lets us break and continue in case // C1 is not a
		 * 'for'.
		 */
		produceFirstIfNeeded(controls, false);

		Literal initialResult = lit(isSome ? false : true);
		VarDecl resultDecl = produce_result_and_loopLabel(initialResult);

		Cmd newResult = new Literal(start, end, isSome ? true : false);

		Seq seq = break_query_with_result(newResult);
		// theIf will be 'if (B) { ¬©result¬© := newResult; break ¬©query¬©; }'
		Cmd B = (Cmd) pred.ingestedDeepCopy(start, end);
		If theIf = new If(start, end, B, seq, null, isSome ? false : true, false);

		Cmd body = basic_outer_body(theIf);

		Cmd queryLoop = controlLoop(loopLabel, controls, body);

		Cmd returnResult = varexp(result);
		return buildUltimateQuery(resultDecl, queryLoop, returnResult);
	}

	public Cmd countDesugaring(List<QueryControl> controls, Cmd pred) {
		// %count(B % C1, C2)
		produceFirstIfNeeded(controls, false);
		// Initially, result := 0
		// (And yes, result is used for the counter)
		Literal initialResult = lit(0);
		VarDecl resultDecl = produce_result_and_loopLabel(initialResult);
		//Increment result when B is true
		Cmd incrResult = assign(result, op(PLUS, varexp(result), lit(1)));
		Cmd ifBIncrResult = iff(pred, incrResult, null);

		Cmd body = basic_outer_body(ifBIncrResult);

		Cmd queryLoop = controlLoop(loopLabel, controls, body);
		Cmd returnResult = varexp(result);
		return buildUltimateQuery(resultDecl, queryLoop, returnResult);
	}

	public Cmd firstDesugaring(List<QueryControl> controls, Cmd E, Cmd N) {
		// %first(E %none N % C1, C2)
		// -->
		// var ¬©first¬© := true;
		// var ¬©result¬©;
		// ¬©query¬©: do {
		//   ~~C1~~
		//     ~~C2~~
		//       ¬©result¬©, @first@ := E, false;
		//       break @query@;
		//  }until(true);
		//  if (¬©first¬©) {N} else {¬©result¬©;}
		// But, if N == null, use "throw 'not found'" instead;
		produceFirstIfNeeded(controls, true);
		Cmd initialResult = null;
		VarDecl resultDecl = produce_result_and_loopLabel(initialResult);

		Cmd setResultAndfirst = assign(first, result, lit(false), E);
		Cmd breaker = break_query();
		Cmd body = seq(setResultAndfirst, breaker);

		Cmd queryLoop = controlLoop(loopLabel, controls, body);

		Cmd NorThrow = N != null ? N : lit(null);

		Cmd returnResult = iff(varexp(first), NorThrow, varexp(result));
		return buildUltimateQuery(resultDecl, queryLoop, returnResult);
	}

	private Cmd afterDesugaring(List<QueryControl> controls, Cmd E) {
		// %after(E % C1, C2)
		// -->
		// {
		// var @first@ := true;
		// @query@: do {
		//    ~~C1~~
		//      ~~C2~~
		//        @first@ := false;
		// }until(true);
		// E;
		// }
		// (But note that var controls put stuff before the @query@, which is crucial.)
		produceFirstIfNeeded(controls, true);
		produce_loopLabel();

		Cmd body = assign(first, lit(false));
		Cmd queryLoop = controlLoop(loopLabel, controls, body);

		Cmd returnResult = (Cmd) E.ingestedDeepCopy(start, end);
		return buildUltimateQuery(null, queryLoop, returnResult);
	}

	private Break break_query() {
		Break breaker = new Break(start, end, (Id) loopLabel.ingestedDeepCopy(start, end));
		return breaker;
	}

	private Cmd buildUltimateQuery(VarDecl resultDecl, Cmd queryLoop, Cmd returnResult) {
		List<Cmd> seqList = new ArrayList<Cmd>();
		if (resultDecl != null)
			seqList.add(resultDecl);
		seqList.addAll(extraStuffForPrelude);
		seqList.add(queryLoop);
		seqList.add(returnResult);
		Seq wholeThing = new Seq(start, end, seqList);
		return wholeThing;
	}

	private Cmd basic_outer_body(Cmd innerBody) {
		Cmd basic_outer_body;
		if (first != null) {
			//			AssignTarget toFirst = new AssignToId(start, end, first.deepCopy(start, end));
			Assign firstGetsFalse = assign(first, lit(false));//new Assign(start, end, list(toFirst), list((Cmd) Lit(false)));
			basic_outer_body = new Seq(start, end, list((Cmd) innerBody, (Cmd) firstGetsFalse));
		} else {
			basic_outer_body = innerBody;
		}
		return basic_outer_body;
	}

	private Seq break_query_with_result(Cmd newResult) {
		AssignTarget toResult = new AssignToId(start, end, (Id)result.ingestedDeepCopy(start, end));
		Assign assignment = new Assign(start, end, list(toResult), list(newResult));
		Break breaker = new Break(start, end, (Id) loopLabel.ingestedDeepCopy(start, end));
		Seq seq = new Seq(start, end, list(assignment, breaker));
		return seq;
	}

	private VarDecl produce_result_and_loopLabel(Cmd result_init) {
		result = DesugarUtils.gensym(start, "result");
		produce_loopLabel();
		final Id id = (Id) result.ingestedDeepCopy(start, end);
		return new VarDecl(start, end, id, result_init == null ? null : (Cmd) result_init
				.ingestedDeepCopy(start, end), tycon(id));
	}

	private void produce_loopLabel() {
		loopLabel = DesugarUtils.gensym(start, "query");
	}

	private boolean produceFirstIfNeeded(List<QueryControl> controls, boolean yesItsNeeded) {
		boolean needsFirstFlag = yesItsNeeded || needsFirstFlag(controls);
		this.first = needsFirstFlag ? DesugarUtils.gensym(start, "first") : null;
		if (needsFirstFlag) {
			final Id id = (Id) first.ingestedDeepCopy(start, end);
			VarDecl firstDecl = new VarDecl(start, end, id, lit(true), tycon(id, "bool"));
			extraStuffForPrelude.add(firstDecl);
		} else {

		}
		return needsFirstFlag;
	};

	public static boolean needsFirstFlag(List<QueryControl> controls) {
		for (QueryControl queryControl : controls) {
			if (queryControl instanceof QueryControlVar) {
				QueryControlVar qcv = (QueryControlVar) queryControl;
				if (qcv.doBeforeFirst)
					continue;
				return true;
			}
		}
		return false;
	}

	private Cmd controlLoop(Id loopLabel, List<QueryControl> controls, Cmd innerBody) {
		Cmd myBody = innerControlLoop(controls, 0, innerBody);
		// Now, wrap loopLabel: do { ... } until(true) around it.
		Cmd theWhile = new While(start, end, loopLabel, lit(true), myBody, true, true);
		return theWhile;
	}

	// i = number of control we're working on.
	private Cmd innerControlLoop(List<QueryControl> controls, int i, Cmd innerBody) {
		if (i >= controls.size()) {
			return innerBody;
		}
		Cmd myBody = innerControlLoop(controls, i + 1, innerBody);
		QueryControl control = controls.get(i);
		// OK, we need to construct ~~control~~ with body 'myBody'.
		Cmd myLoop = control.accept(this, myBody);
		return myLoop;
	}

	public Cmd visit(QueryControl syn, Cmd arg) {
		Doom.notYet();
		return null;
		// 
	}

	public Cmd visit(QueryControlFor syn, Cmd arg) {
		Pat dpat = (Pat) syn.pat.ingestedDeepCopy(syn.start, syn.end);
		Cmd dlist = (Cmd) syn.list.ingestedDeepCopy(syn.start, syn.end);
		Cmd dbody = arg;
		return new For(syn.start, syn.end, null, dpat, dlist, dbody, syn.inquisitive);
	}

	public Cmd visit(QueryControlIf syn, Cmd arg) {
		Token start = syn.start;
		Token end = syn.end;
		Cmd test = (Cmd) syn.pred.ingestedDeepCopy(start, end);
		Cmd Then = arg;
		Cmd Else = null;
		If iff = new If(start, end, test, Then, Else, false, false);
		return iff;
	}

	public Cmd visit(QueryControlVal syn, Cmd arg) {
		// expansion is:
		// { syn.pat = syn.exp ; arg }
		Token start = syn.start;
		Token end = syn.end;
		Bind bind = new Bind(start, end, syn.pat, syn.exp);
		Seq seq = new Seq(start, end, list((Cmd) bind, arg));
		return seq;
	}

	public Cmd visit(QueryControlVar syn, Cmd arg) {
		Token start = syn.start;
		Token end = syn.end;
		final Id id = (Id) syn.var.ingestedDeepCopy(start, end);
		if (syn.doBeforeFirst) {
			// init: var v := init
			VarDecl init = new VarDecl(start, end, id, (Cmd) syn.init.ingestedDeepCopy(start, end), tycon(id));
			extraStuffForPrelude.add(init);
			// body: {v := next; arg}
			AssignTarget v_body = new AssignToId(start, end, id);
			Assign vGetsNext = new Assign(start, end, list(v_body), list(syn.next));
			Seq seq = new Seq(start, end, list((Cmd) vGetsNext, arg));
			return seq;
		} else {
			// initial decl
			VarDecl vd = new VarDecl(start, end, id, null, tycon(id));
			extraStuffForPrelude.add(vd);

			// In loop: { if (¬©first¬©) {v := init} else {v := next}    arg }
			//AssignTarget v1 = new AssignToId(start, end, syn.var.deepCopy(start, end));
			Assign whenFirst = assign(syn.var, syn.init);//new Assign(start, end, list(v1), list((Cmd) syn.init.deepCopy(start, end)));

			//AssignTarget v2 = new AssignToId(start, end, syn.var.deepCopy(start, end));
			Assign notFirst = assign(syn.var, syn.next);//new Assign(start, end, list(v2), list((Cmd) syn.next.deepCopy(start, end)));

			VarExp testFirst = varexp(first); // new VarExp(start, end, first.deepCopy(start, end));

			If iff = iff(testFirst, whenFirst, notFirst); // new If(start, end, testFirst, whenFirst, notFirst, false, false);

			Seq seq = seq(iff, arg); // new Seq(start, end, list(iff, arg));
			return seq;
		}
	}

	public Cmd visit(QueryControlWhile syn, Cmd arg) {
		// while B  -->  unless (B) {break @query@;}
		Token start = syn.start;
		Token end = syn.end;
		Cmd breakq = new Break(start, end, (Id) loopLabel.ingestedDeepCopy(start, end));
		If unless = new If(start, end, (Cmd) syn.test.ingestedDeepCopy(start, end), breakq, null, true, false);
		Seq seq = new Seq(start, end, list(unless, arg));
		return seq;
	}

	private Cmd tableDesugaring(List<QueryControl> controls, List<IdWithOptInit> keys, List<TableFields> items) {
		// %table(k1=e1, k2=e2){ f1 = v1; var f2 := v2; % controls }
		// --->
		// {
		// t = table(k1, k2){f1, var f2};
		// ~~controls~~
		//    t.ins({: k1: e1, k2: e2, f1: v1, f2: v2 :});
		// t;
		// }
		Id t = DesugarUtils.gensym(start, "t");

		// COMPUTE t = table(k1, k2){f1, var f2};
		List<TableKey> keysT = new ArrayList<TableKey>();

		List<RecordField> recFields = new ArrayList<RecordField>(); // This is for the loop body.

		for (IdWithOptInit idwi : keys) {
			TableKey tk = new TableKey(start, end, idwi.id, tycon());
			keysT.add(tablekey(tk));
			RecordField recfield = rf(idwi.id, idwi.init);
			if (idwi.init == null)
				recfield.flag(DangerLevel.ERROR, Ingester.TABLE_QUERY_FIELDS_NEED_INITIALIZERS + ": " + idwi.id,
						"Field = " + idwi.id);
			recFields.add(recfield);
		}

		List<TableFields> vals = new ArrayList<TableFields>();
		for (TableFields tf : items) {
			List<IdWithOptInit> iwois = new ArrayList<IdWithOptInit>(tf.idInits.size());
			for (IdWithOptInit iwoi : tf.idInits) {
				IdWithOptInit iwoi4t = new IdWithOptInit(iwoi.start, iwoi.end, id(iwoi.id), null, tyclone(iwoi.typeConstraints), iwoi.colAccess);
				iwois.add(iwoi4t);
				recFields.add(rf(iwoi.id, iwoi.init));
			}
			TableFields tft = new TableFields(start, end, tf.colAccess, tf.colSpecial, iwois);
			vals.add(tft);
		}

		Table tableExp = new Table(start, end, keysT, vals);

		Bind declareT = bind(t, tableExp);
		// AND THAT'S t = table(k1, k2){f1, var f2};

		// COMPUTE t.ins({: k1: e1, k2: e2, f1: v1, f2: v2 :});

		RecordCtor rec = new RecordCtor(start, end, recFields);
		MethodCall tDotIns = meth(varexp(id(t)), "ins", rec);

		produceFirstIfNeeded(controls, true);
		produce_loopLabel();

		Cmd body = tDotIns;
		Cmd mainLoop = controlLoop(loopLabel, controls, body);

		// AND THAT'S  t.ins({: k1: e1, k2: e2, f1: v1, f2: v2 :});
		// 

		Cmd returnT = varexp(t);

		Seq seq = seq(declareT, mainLoop, returnT);
		return seq;
	}

	private Cmd groupDesugaring(List<QueryControl> controls, List<QGKey> keys, List<QGAccum> accums) {
		// %group(a=ka){map b= %first b1 %then b2 %after b3 % controls}
		// -->
		// ¬©t¬© = table(a){map b};
		// ~~controls~~
		//   ©a = ka;
		//   if (¬©t¬©(©a) ~ {:  b :}) {
		//     ¬©t¬©(©a) := {: b : b2 :};
		//     }
		//   else {
		//     ¬©t¬©(©a) := {: b : b1 :};
		//   }
		// for({: a, b :} <- ¬©t¬©) {
		//   ¬©t¬©(a) := {: b : b3 :};
		// }

		// Whew.

		// %group(a=ka){map b= %first b1 %then b2 %after b3 % controls}
		Id t = DesugarUtils.gensym(start, "t");
		List<Id> genKey = DesugarUtils.gensymEach(start, keys, "genKey");
//		System.out.println("genKey=" + Bard.sep(genKey, " + "));
		Cmd makeT = groupMakeT(controls, keys, accums, genKey, t);
		Cmd mainLoop = groupMainLoop(controls, keys, accums, genKey, t);
		Cmd afterbits = groupAfterbits(controls, keys, accums, t);
		Seq seq = seq(makeT, mainLoop, afterbits, varexp(t));

//		System.out.println("---groupDesugaring: \n" + seq +
//				"\n controls = " + Bard.sep(controls, "  ★  ") +
//				"\n keys = " + Bard.sep(keys, "  ± ") +
//				"\n accums = " + Bard.sep(accums, " ◣ ")
//				
//				);

		return seq;
	}

	private Cmd groupMakeT(List<QueryControl> controls, List<QGKey> keys, List<QGAccum> accums, List<Id> genKey, Id t) {
		// ¬©t¬© = table(a){map b};
		List<TableKey> tkeys = list();
		for (QGKey qgkey : keys) {
			TableKey tk = new TableKey(start, end, qgkey.id, tycon());
			tkeys.add(tk);
		}

		List<TableFields> tfields = list();
		for (QGAccum accum : accums) {
			IdWithOptInit iwoi = new IdWithOptInit(start, end, accum.id, null, tycon(), accum.colAccess);
			TableFields tf = new TableFields(start, end, accum.colAccess, accum.colSpecial, list(iwoi));
			tfields.add(tf);
		}

		Table table = new Table(start, end, tkeys, tfields);
		Bind tIsTable = bind(t, table);

//		System.out.println("---groupMakeT:\n" + tIsTable);

		return tIsTable;
	}

	private Cmd groupMainLoop(List<QueryControl> controls, List<QGKey> keys, List<QGAccum> accums, List<Id> genKey, Id t) {
		// ~~controls~~
		//   Ca = ka;
		//   if (¬©t¬©(Ca) ~ {:  b :}) {
		//     ¬©t¬©(Ca) := {: b : b2 :};
		//     }
		//   else {
		//     ¬©t¬©(Ca) := {: b : b1 :};
		//   }

		List<Cmd> heartCmds = list();

		// Keys induce bindings...
		//   Ca = ka;
		int i = 0;
		for (QGKey key : keys) {
			Id gennedKey = genKey.get(i++);
			Bind keyBind = bind(gennedKey, key.init);
			heartCmds.add(keyBind);
		}

		// Build ¬©t¬©(Ca) ~ {:  b :} for the if test.
		List<Cmd> keySubscripts = list();
		// We'll need a few copies.
		List<Cmd> keySubscripts2 = list();
		for (Id gennedkey : genKey) {
			keySubscripts.add(varexp(gennedkey));
			keySubscripts2.add(varexp(gennedkey));
		}

		FunCall tSubKeys = new FunCall(start, end, varexp(t), keySubscripts);

		List<PatRecordField> matchFields = list();
		List<RecordField> thenFields = list();
		List<RecordField> firstFields = list();
		for (QGAccum accum : accums) {
			matchFields.add(new PatRecordField(start, end, id(accum.id), patvar(accum.id)));
			thenFields.add(rf(accum.id, accum.then));
			firstFields.add(rf(accum.id, accum.first));
		}

		Pat accumRecPat = new PatRecordCtor(start, end, matchFields);

		Cmd retrieveAndBind = new MatchExp(start, end, tSubKeys, accumRecPat);

		// ¬©t¬©(Ca) := {: b : b2 :}; -- do stuff to the after clauses.

		AssignToSubscripted tSubKeys2AssTarg = new AssignToSubscripted(start, end, varexp(t), keySubscripts2);
		AssignToSubscripted tSubKeys2AssTarg2 = (AssignToSubscripted) tSubKeys2AssTarg.ingestedDeepCopy(start, end);
		
		RecordCtor thenMutation = new RecordCtor(start, end, thenFields);

		Cmd afterly = new Assign(start, end, list((AssignTarget)tSubKeys2AssTarg), list((Cmd)thenMutation));

		// ¬©t¬©(Ca) := {: b : b1 :};
		
		RecordCtor firstMutation = new RecordCtor(start, end, firstFields);
		Cmd firstly = new Assign(start, end, list((AssignTarget)tSubKeys2AssTarg2), list((Cmd)firstMutation));

		If iff = iff(retrieveAndBind, afterly, firstly);
		
		heartCmds.add(iff);

//		System.out.println("---groupMainLoop if\n" + iff);

		produceFirstIfNeeded(controls, true);
		produce_loopLabel();

		Cmd heart = new Seq(start, end, heartCmds);
//		System.out.println("heart=" + heart);

		Cmd mainLoop = controlLoop(loopLabel, controls, heart);

//		System.out.println("---groupMainLoop\n" + mainLoop);

		return mainLoop;

	}

	private Cmd groupAfterbits(List<QueryControl> controls, List<QGKey> keys, List<QGAccum> accums,  Id t) {
		// This can be omitted if no field has an %after clause.
		// for({: a:Ca, b :} <- ¬©t¬©) {
		//   ¬©t¬©(Ca) := {: b : b3 :};
		// }
		
		// A second set of key variables, disjoint from the first set.
		List<Id> genKey2 = DesugarUtils.gensymEach(start, keys, "2-2-2-2-2");
		
		// Check for an %after clause:
		// (This would be so slick with a %every in Java, ne?)
		boolean hasAfter = false;
		for (QGAccum accum2 : accums) {
			if (accum2.after != null) {
				hasAfter = true;
				break;
			}
		}
		if (! hasAfter) {
			return seq();
		}
		
		
		Id row = DesugarUtils.gensym(start, "row");
		
		List<PatRecordField> columnsForRowPat = list();
		List<Cmd> subscripts = list();
		int i = 0;
		for (QGKey key : keys) {
			Id gennedKey = genKey2.get(i++);
			columnsForRowPat.add(new PatRecordField(start, end, id(key.id), patvar(gennedKey)));
			subscripts.add(varexp(gennedKey));
		}
		
		List<RecordField> afterFields = list();
		for (QGAccum accum : accums) {
			columnsForRowPat.add(new PatRecordField(start, end, id(accum.id), patvar(accum.id)));
			if (accum.after != null) {
				afterFields.add(rf(accum.id, accum.after));
			}
			else {
				afterFields.add(rf(accum.id, varexp(accum.id)));
			}
		}
		
		PatRecordCtor rowPat = new PatRecordCtor(start, end, columnsForRowPat);
		
		AssignToSubscripted tSubKeysAssTarg = new AssignToSubscripted(start, end, varexp(t), subscripts);
		
		RecordCtor afterRow = new RecordCtor(start, end, afterFields);
		
		Cmd forBody = new Assign(start, end, list((AssignTarget) tSubKeysAssTarg), list((Cmd) afterRow));
		
		Cmd tRows = meth(varexp(t), "rows");
		
		For forrow = new For(start, end, null, rowPat, tRows, forBody, false);
		
//		System.out.println("---groupAfterbits forrow \n" + forrow);
		
		return forrow;
		
	}

}
