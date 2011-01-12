
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.syn.core;

import static fisher.syn.core.Fixity.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.Frame;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.BoolTh;
import fisher.runtime.FloatTh;
import fisher.runtime.IntRangeTh;
import fisher.runtime.IntTh;
import fisher.runtime.Internal_SortableTh;
import fisher.runtime.ListTh;
import fisher.runtime.StringTh;
import fisher.runtime.Thing;
import fisher.syn.Cmd;
import fisher.syn.OpExp;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;

public enum Op {
	PLUS("+", NARY, true) {
		@Override
		public Thing simpleEval(Thing[] actuals, Syntax syn, Framelike frame) throws FisherException {
			assert (actuals.length == 2);
			if (actuals[0] == null)
				Doom.nullPointer(Evaller.mine(), syn);
			return actuals[0].invokeMethod("+", new Thing[] { actuals[1] },  syn);

			//			if (EvalUtil.allLongs(actuals)) {
			//				long sum = 0;
			//				long[] actlongs = EvalUtil.asLongs(actuals, syn);
			//				for (long l : actlongs) {
			//					sum += l;
			//				}
			//				return IntTh.of(sum);
			//			} else if (EvalUtil.allNumbers(actuals)) {
			//				double sum = 0;
			//				double[] acts = EvalUtil.asDoubles(actuals, syn);
			//				for (double d : acts) {
			//					sum += d;
			//				}
			//				return FloatTh.of(sum);
			//			} else if (actuals[0] instanceof StringTh) {
			//				StringBuffer sb = new StringBuffer();
			//				for (Thing thing : actuals) {
			//					sb.append(EvalUtil.toString(thing));
			//				}
			//				return StringTh.of(sb.toString());
			//			} else {
			//				assert (actuals.length == 2);
			//				return actuals[0].invokeMethod("+", new Thing[] { actuals[1] }, Evaller.mine(), frame, syn);

			//			}
			/*
			 * 
			 if (EvalUtil.anyString(actuals)) {
			StringBuffer sb = new StringBuffer();
			for (Thing thing : actuals) {
				sb.append(EvalUtil.toString(thing));
			}
			return StringTh.of(sb.toString());
			}
			else {
			return Doom.notYet("Plus as a method call");
			} */
		}// simpleEval
	}, // PLUS
	MINUS("-", BIN, false) {
		@Override
		public Thing simpleEval(Thing[] actuals, Syntax syn, Framelike frame) throws FisherException {
			assert (actuals.length == 2);
			if (actuals[0] == null)
				Doom.nullPointer(Evaller.mine(), syn);
			return actuals[0].invokeMethod("-", new Thing[] { actuals[1] },  syn);
			//			if (EvalUtil.allLongs(actuals)) {
			//				long sum = actuals[0].asLong(syn);
			//				for (int i = 1; i < actuals.length; i++)
			//					sum -= actuals[i].asLong(syn);
			//				return IntTh.of(sum);
			//			} else if (EvalUtil.allNumbers(actuals)) {
			//				double sum = actuals[0].asDouble(syn);
			//				for (int i = 1; i < actuals.length; i++)
			//					sum -= actuals[i].asDouble(syn);
			//				return FloatTh.of(sum);
			//			} else {
			//				return Doom.notYet("Minus as a method call");
			//			}
		}// simpleEval
	},
	NEG("-", PRE, false) {
		@Override
		public Thing simpleEval(Thing[] actuals, Syntax syn, Framelike frame) throws FisherException {
			sizeCheck(actuals, 1, 1, "Negation (-X) requires exactly one argument.", syn, frame);
			if (actuals[0] == null)
				Doom.nullPointer(Evaller.mine(), syn);
			if (EvalUtil.allLongs(actuals)) {
				return IntTh.of(-(actuals[0].asLong(syn)));
			} else if (EvalUtil.allNumbers(actuals)) {
				return FloatTh.of(-(actuals[0].asDouble(syn)));
			} else {
				return Doom.notYet("Unary negation as a method call");
			}
		}
	}, // NEG
	POS("+", PRE, false) {
		@Override
		public Thing simpleEval(Thing[] actuals, Syntax syn, Framelike frame) throws FisherException {
			sizeCheck(actuals, 1, 1, "Positive (+X) requires exactly one argument.", syn, frame);
			Thing thing = actuals[0];
			return EvalUtil.enplus(thing);
		}
	}// POS
	,
	NOT("!", PRE, false) {
		@Override
		public Thing simpleEval(Thing[] actuals, Syntax syn, Framelike frame) throws FisherException {
			if (actuals[0] == null)
				Doom.nullPointer(Evaller.mine(), syn);
			sizeCheck(actuals, 1, 1, "Negation (!X, not X) requires exactly one argument.", syn, frame);
			if (actuals[0].isBoolean()) {
				boolean b = actuals[0].asBoolean(syn);
				return BoolTh.of(!b);
			} else {
				return Doom.notYet("Boolean negation as a method call");
			}
		}
	}// NOT
	,
	TIMES("*", NARY, true) {
		@Override
		public Thing simpleEval(Thing[] actuals, Syntax syn, Framelike frame) throws FisherException {
			if (actuals[0] == null)
				Doom.nullPointer(Evaller.mine(), syn);
			if (EvalUtil.allLongs(actuals)) {
				long prod = 1;
				long[] actlongs = EvalUtil.asLongs(actuals, syn);
				for (long l : actlongs) {
					prod *= l;
				}
				return IntTh.of(prod);
			} else if (EvalUtil.allNumbers(actuals)) {
				double sum = 1;
				double[] acts = EvalUtil.asDoubles(actuals, syn);
				for (double d : acts) {
					sum *= d;
				}
				return FloatTh.of(sum);
			} else {
				assert (actuals.length == 2);
				return actuals[0].invokeMethod("*", new Thing[] { actuals[1] },  syn);
			}
		}// simpleEval

	}// TIMES
	,
	FDIV("/", BIN, false) {
		@Override
		public Thing simpleEval(Thing[] actuals, Syntax syn, Framelike frame) throws FisherException {
			if (actuals[0] == null)
				Doom.nullPointer(Evaller.mine(), syn);
			if (EvalUtil.allNumbers(actuals)) {
				if (actuals.length == 2) {
					double num = actuals[0].asDouble(syn);
					double den = actuals[1].asDouble(syn);
					if (den == 0)
						Doom.runtime("Division by zero", syn, actuals);
					return FloatTh.of(num / den);
				} else {
					Doom.internal("Non-binary division?", syn, actuals);
					return null;
				}
			} else {
				assert (actuals.length == 2);
				return actuals[0].invokeMethod("/", new Thing[] { actuals[1] },  syn);
			}
		}
	}// FDIV
	,
	IDIV(" div ", BIN, false) {
		@Override
		public Thing simpleEval(Thing[] actuals, Syntax syn, Framelike frame) throws FisherException {
			sizeCheck(actuals, 2, 2, "Integer division (X div Y) requires exactly two arguments.", syn, frame);
			//			if (EvalUtil.allLongs(actuals)) {
			//				long num = actuals[0].asLong(syn);
			//				long den = actuals[1].asLong(syn);
			//				if (den == 0) {
			//					return Doom.runtime("Divison by zero", syn, num, frame);
			//				} else {
			//					return IntTh.of(num / den);
			//				}
			//			} else {
			if (actuals[0] == null)
				Doom.nullPointer(Evaller.mine(), syn);
			assert (actuals.length == 2);
			return actuals[0].invokeMethod("div", new Thing[] { actuals[1] },  syn);
			//			}
		}// simpleEval
	}// IDIV
	,
	MOD(" mod ", BIN, false) {
		@Override
		public Thing simpleEval(Thing[] actuals, Syntax syn, Framelike frame) throws FisherException {
			if (actuals[0] == null)
				Doom.nullPointer(Evaller.mine(), syn);
			assert (actuals.length == 2);
			return actuals[0].invokeMethod("mod", new Thing[] { actuals[1] },  syn);
		}// simpleEval
	}// MOD
	,
	ONEOF("^^", NARY, true) {
		@Override
		public Thing eval(OpExp syn, Framelike frame, Evaller evaller) throws FisherException {
			int nTrue = 0;
			
			for (Cmd junct : syn.operands) {
				Thing th = evaller.eval(junct, frame);
				if (th == null) {
					Doom.runtime("Conjuncts should evaluate to booleans, not null", junct, syn);
				} else if (th.isBoolean()) {
					boolean b = th.asBoolean(junct);
					if (b) {
						nTrue++;
					}
					if (nTrue > 1)
						break;
				} else {
					Doom.runtime("^^'ed values should evaluate to booleans, not " + th.typeString(), junct, syn, th);
				}
			}
			return BoolTh.of(nTrue == 1);
		}
	}// ONEOF
	,
	AND("&&", NARY, true) {
		@Override
		public boolean isConjunction() {
			return true;
		}

		@Override
		public Thing eval(OpExp syn, Framelike frame, Evaller evaller) throws FisherException {
			for (Cmd junct : syn.operands) {
				Thing th = evaller.eval(junct, frame);
				if (th == null) {
					Doom.runtime("Conjuncts should evaluate to booleans, not null", junct, syn);
				} else if (th.isBoolean()) {
					boolean b = th.asBoolean(junct);
					if (!b)
						return BoolTh.False;
				} else {
					Doom.runtime("Conjuncts should evaluate to booleans, not " + th.typeString(), junct, syn, th);
				}
			}
			return BoolTh.True;
		}
	},
	OR("||", NARY, true) {
		@Override
		public Thing eval(OpExp syn, Framelike frame, Evaller evaller) throws FisherException {
			for (Cmd junct : syn.operands) {
				Thing th = evaller.eval(junct, frame);
				if (th == null) {
					Doom.runtime("Disjuncts should evaluate to booleans, not null", junct, syn);
				} else if (th.isBoolean()) {
					boolean b = th.asBoolean(junct);
					if (b)
						return BoolTh.True;
				} else {
					Doom.runtime("Disjuncts should evaluate to booleans, not " + th.typeString(), junct, syn, th);
				}
			}
			return BoolTh.False;
		}

		public boolean shouldBeSealedIndependently() {
			return true;
		}
	}// OR
	,
	DOTDOT("..", BIN, false) {
		@Override
		public Thing simpleEval(Thing[] actuals, Syntax syn, Framelike frame) throws FisherException {
			sizeCheck(actuals, 2, 2, "The '..' operator must take two arguments", syn, frame);
			if (actuals[0] == null) Doom.nullPointer(Evaller.mine(), syn);
			if (actuals[1] == null) Doom.nullPointer(Evaller.mine(), syn);
			if (EvalUtil.allLongs(actuals)) {
				return IntRangeTh.of(actuals[0].asLong(syn), actuals[1].asLong(syn));
			} else {
				return actuals[0].invokeMethod("..", Bard.array(actuals[1]), syn);
			}
		}
	}// DOTDOT
	,
	DOTDOTLT("..<", BIN, false) {
	}// DOTDOTLT
	,
	APPEND("@", BIN, true) {
		@Override
		public Thing simpleEval(Thing[] actuals, Syntax syn, Framelike frame) throws FisherException {
			if (allListsOrNulls(actuals)) {
				ListTh appendage = ListTh.EMPTY;
				for (int i = actuals.length - 1; i >= 0; i--) {
					final Thing thing2 = actuals[i];
					if (thing2 != null) {
						ListTh thing = thing2.asList(syn);
						appendage = appendage.appendBehind(thing);
					}
					else {
						Doom.nullPointer(Evaller.mine(), syn);
					}
				}
				return appendage;
			} else {
				return Doom.notYet("@ is not yet defined on much but lists.");
			}

		}
	}// APPEND
	,
	CONS("::", BIN, false) {
		@Override
		public Thing simpleEval(Thing[] actuals, Syntax syn, Framelike frame) throws FisherException {
			if (actuals.length != 2) {
				Doom.internal(":: demands precisely two arguments", syn, actuals);
			}
			Thing newcar = actuals[0];
			if (actuals[1] == null) Doom.nullPointer(Evaller.mine(), syn);
			ListTh newcdr = actuals[1].asList(syn);
			return newcdr.cons(newcar);
		}
	}// CONS
	,
	REV_CONS("(rev_cons has no print rep)", BIN, false) {
		@Override
		public Thing simpleEval(Thing[] actuals, Syntax syn, Framelike frame) throws FisherException {
			if (actuals.length != 2) {
				Doom.internal("rev_cons demands precisely two arguments", syn, actuals);
			}
			Thing newcar = actuals[1];
			if (actuals[0] == null) Doom.nullPointer(Evaller.mine(), syn);
			ListTh newcdr = actuals[0].asList(syn);
			return newcdr.cons(newcar);
		}
		@Override
		public boolean internal() {
			// TODO Auto-generated method stub
			return true;
		}
	}// REV_CONS
	
	,
	ADDTO("@=", BIN, false) {
		@Override
		public Thing eval(OpExp syn, Framelike frame, Evaller evaller) throws FisherException {
			return this.evalAsMethod(syn, frame, evaller);
		}
	}, // ADDTO
	DELFROM("\\=", BIN, false) {
		@Override
		public Thing eval(OpExp syn, Framelike frame, Evaller evaller) throws FisherException {
			return this.evalAsMethod(syn, frame, evaller);
		}
	}, // DELFROM
	// There's no reason ops need to be made public...
	INTERNAL_REVERSE("ïñ†érñål_reverse", PRE, false) {
		public Thing simpleEval(Thing[] actuals, Syntax syn, Framelike frame) throws FisherException {
			if (actuals.length != 1)
				Doom.internal("internal_reverse is unary", syn);
			ListTh L = actuals[0].asList(syn);
			return L.reversed();
		}

		@Override
		public boolean internal() {
			return true;
		}
	},
	INTERNAL_SORTABLE("©©ïñ†érñål_sortable©©", NARY, false) {
		@Override
		public Thing simpleEval(Thing[] actuals, Syntax syn, Framelike frame) throws FisherException {
			// actuals[0] = payload
			// actuals[1] = ListTh of ascending/descendings.
			// actuals[2] through the end = keys
			// syn = src
			Thing payload = actuals[0];
			ListTh ascend = actuals[1].asList(syn);
			BoolTh[] ascending = new BoolTh[ascend.length];
			int i = 0;
			for (Thing a : ascend) {
				ascending[i++] = a.asBoolTh(syn);
			}
			Thing[] keys = new Thing[ascend.length];
			if (actuals.length != ascending.length + 2) {
				Doom.internal("Malformed call to INTERNAL_SORTABLE", syn, payload, ascend, ascending, keys, actuals);
			}
			for (int j = 0; j < keys.length; j++) {
				keys[j] = actuals[j + 2];
			}
			Internal_SortableTh isth = new Internal_SortableTh(payload, keys, ascending, syn);
			return isth;
		}

		@Override
		public boolean internal() {
			return true;
		}
	},
	INTERNAL_SORT("©©ïñ†érñål_sort©©", PRE, false) {
		@Override
		public Thing simpleEval(Thing[] actuals, Syntax src, Framelike frame) throws FisherException {
			if (actuals.length != 1)
				Doom.internal("internal_sort malformed", src, (Object[])actuals);
			ListTh thingsToSort = actuals[0].asList(src);
			List<Internal_SortableTh> jl = (List<Internal_SortableTh>) (List) thingsToSort.toJavaList();
			Collections.sort(jl);
			// Extract payloads into a list.
			ListTh sorted = ListTh.EMPTY;
			for (int j = jl.size() - 1; j >= 0; j--) {
				sorted = sorted.cons(jl.get(j).payload);
			}
			return sorted;
		}

		@Override
		public boolean internal() {
			return true;
		}
	};

	public boolean internal() {
		return false;
	}

	public boolean isConjunction() {
		return false;
	}

	public Thing eval(OpExp syn, Framelike frame, Evaller evaller) throws FisherException {
		Thing[] actuals = evaller.evalAll(syn.operands, frame);
		Thing res = simpleEval(actuals, syn, frame);
		return res;
	}

	public Thing evalAsMethod(OpExp syn, Framelike frame, Evaller evaller) throws FisherException {
		Thing receiver = evaller.eval(syn.operands.get(0), frame);
		Thing[] args = evaller.evalAllButFirst(syn.operands, frame);
		if (receiver == null) {
			Doom.nullPointer(evaller, syn);
		}
		Thing res = receiver.invokeMethod(this.str, args,  syn);
		return res;
	}

	public Thing simpleEval(Thing[] actuals, Syntax syn, Framelike frame) throws FisherException {
		Doom.internal("This operation is either not yet implemented, or not simple", syn, frame);
		return EvalUtil.nip(syn);
	}

	public static final int VARIADIC = -1; // Really this should belong to
	// Fixity.

	public final String str;
	public final Fixity fixity;
	public final boolean associative;

	Op(String str, Fixity fixity, boolean associative) {
		this.str = str;
		this.fixity = fixity;
		this.associative = associative;
	}

	public String toString() {
		return this.str;
	}

	public boolean numberOfArgumentsIsRight(List<? extends Syntax> operands) {
		return (fixity.nArgs == VARIADIC || operands.size() == fixity.nArgs);
	}

	private static List<Op> cacheBinaries = null;

	public static List<Op> binariable() {
		if (cacheBinaries == null) {
			cacheBinaries = new ArrayList<Op>(10);
			for (Op op : Op.values()) {
				if (op.fixity == BIN || op.fixity == NARY) {
					if (!op.internal()) {
						cacheBinaries.add(op);
					}
				}
			}
		}
		return cacheBinaries;
	}

	public boolean shouldBeSealedIndependently() {
		return false;
	}

	protected static <T> void sizeCheck(T[] stuff, int min, int max, String msg, Syntax src, Framelike frame)
			throws FisherException {
		if (stuff.length < min || stuff.length > max) {
			Doom.runtime(msg, src, frame);
		}
	}

	private static boolean allListsOrNulls(Thing[] things) {
		for (Thing thing : things) {
			if (thing == null)
				continue;
			if (!thing.isList())
				return false;
		}
		return true;
	}

}
