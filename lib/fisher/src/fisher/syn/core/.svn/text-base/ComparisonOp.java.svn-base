
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

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.BuiltInType;
import fisher.runtime.ClassDynamicTh;
import fisher.runtime.ListTh;
import fisher.runtime.ObjectTh;
import fisher.runtime.Thing;
import fisher.syn.OpExp;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;

public enum ComparisonOp {
	LE("≤", "<=") {
		@Override
		public boolean compare(Thing left, Thing right, Syntax src, Framelike frame, Evaller evaller)
				throws FisherException {
			return EvalUtil.compare(left, right, src) <= 0;
		}
	},
	LT("<") {
		@Override
		public boolean compare(Thing left, Thing right, Syntax src, Framelike frame, Evaller evaller)
				throws FisherException {
			return EvalUtil.compare(left, right, src) < 0;
		}
	},
	GE("≥", ">=") {
		@Override
		public boolean compare(Thing left, Thing right, Syntax src, Framelike frame, Evaller evaller)
				throws FisherException {
			return EvalUtil.compare(left, right, src) >= 0;
		}
	},
	GT(">") {
		@Override
		public boolean compare(Thing left, Thing right, Syntax src, Framelike frame, Evaller evaller)
				throws FisherException {
			return EvalUtil.compare(left, right, src) > 0;
		}
	},
	EQ("==") {
		@Override
		public boolean compare(Thing left, Thing right, Syntax src, Framelike frame, Evaller evaller)
				throws FisherException {
			return EvalUtil.compare(left, right, src) == 0;
		}
	},
	NE("≠", "!=") {
		@Override
		public boolean compare(Thing left, Thing right, Syntax src, Framelike frame, Evaller evaller)
				throws FisherException {
			return EvalUtil.compare(left, right, src) != 0;
		}
	},
	TYPE(":?") {
		@Override
		public boolean compare(Thing left, Thing right, Syntax src, Framelike frame, Evaller evaller)
				throws FisherException {
			if (right instanceof ClassDynamicTh) {
				ClassDynamicTh cd = (ClassDynamicTh) right;
				if (left instanceof ObjectTh) {
					ObjectTh obj = (ObjectTh) left;
					return cd.hasInstance(obj);
				} else {
					return false;
				}
			} else if (right instanceof BuiltInType) {
				BuiltInType bit = (BuiltInType) right;
				final boolean hasInstance = bit.hasInstance(left);
				return hasInstance;
			} else {
				Doom.notYet(":? the other way");
			}
			return false;
		}
	},
	IN("in") {
		@Override
		public boolean compare(Thing left, Thing right, Syntax src, Framelike frame, Evaller evaller)
				throws FisherException {
			if (right == null) {
				return false;
			} else if (EvalUtil.isIter(right)) {
				for (Thing thing : EvalUtil.iter(right, src)) {
					if (EvalUtil.eq(left, thing))
						return true;
				}
				return false;
			} else {
				// This is unusual -- "in" calls a method on the collection (right) argument,
				// not the left.   Collections are more likely to know about elements than vice-versa.
				Thing inned = right.invokeMethod("revIn", new Thing[] { (left) },  src);
				return inned.asBoolean(src);
			}
		}
	};
	public final String text;
	public final String[] choices;

	ComparisonOp(String... choices) {
		this.choices = choices;
		this.text = choices[0];
	}

	public String toString() {
		return this.text;
	}

	public abstract boolean compare(Thing left, Thing right, Syntax src, Framelike frame, Evaller evaller)
			throws FisherException;

	public static ComparisonOp of(String image) {
		for (ComparisonOp c : ComparisonOp.values()) {
			for (String ch : c.choices) {
				if (ch.equals(image))
					return c;
			}
		}
		throw new RuntimeException("Unknown ComparisonOp: " + image);
	}

}
