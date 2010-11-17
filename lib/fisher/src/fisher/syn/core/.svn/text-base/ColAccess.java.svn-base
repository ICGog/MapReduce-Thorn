
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

public enum ColAccess {
	VAR("var", ":=", "assignment"),
	VAL("val", "=", "binding"),
	KEY("key", "=", "binding");
	public String text;
	public String op;
	public String whatis;
	ColAccess(String text, String op, String whatis){this.text = text; this.op = op; this.whatis=whatis;}
	public String toString() {
		return this.text;
	}
	
}
