
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

public enum OpAB {
	PLUSAB("+=", Op.PLUS),
	MINUSAB("-=", Op.MINUS),
	TIMESAB("*=", Op.TIMES),
	FDIVAB("/=", Op.FDIV),
	CONSAB("::=", Op.REV_CONS)
	;
	public String str;
	public Op op;
	private OpAB(String str, Op op) {
		this.str = str;
		this.op = op;
	}
	public String toString() {return this.str;}
	
}
