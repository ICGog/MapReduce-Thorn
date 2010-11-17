
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

import java.util.List;

import fisher.util.Bard;

public enum Fixity {
	BIN(2) {
		@Override
		public String format(List<? extends Syntax> operands, Op op) {
			return operands.get(0).toString() + op + operands.get(1);
		}
		@Override
		public String formatDetails(List<? extends Syntax> operands, Op op) {
			return "(" + operands.get(0).details() + op + operands.get(1).details() + ")"; 
		}
	},
	
	
	PRE(1){
		@Override
		public String format(List<? extends Syntax> operands, Op op) {
			return op.toString() + operands.get(0);
		}
		@Override
		public String formatDetails(List<? extends Syntax> operands, Op op) {
			return "(" + op.toString() + operands.get(0).details() + ")";
		}
	},
	
	NARY(Op.VARIADIC){
		@Override
		public String format(List<? extends Syntax> operands, Op op) {
			return Bard.sep(operands, op.str);
		}
		@Override
		public String formatDetails(List<? extends Syntax> operands, Op op) {
			return "(" + Syntax.sepDetails(operands, op.str) + ")";
		}
	}
	
	;
	

	
	public abstract String format(List<? extends Syntax> operands, Op op);
	public abstract String formatDetails(List<? extends Syntax> operands, Op op);
	public final int nArgs;
	
	Fixity(int nArgs) {
		this.nArgs = nArgs;
	}
}
