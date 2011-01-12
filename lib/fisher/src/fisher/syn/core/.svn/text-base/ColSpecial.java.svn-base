
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

public enum ColSpecial {
	NORMAL("", ""),
	MAP("map", " "),
	ORD("ord", " ");
	public final String name;
	public final String afterspace;

	private ColSpecial(String name, String afterspace) {
		this.name = name;
		this.afterspace = afterspace;
	}
	public String toString() {
		return name;
	}
	public String asPrefix() { return name + afterspace; }
}
