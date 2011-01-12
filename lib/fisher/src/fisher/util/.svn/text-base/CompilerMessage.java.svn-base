
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.util;

import java.util.ArrayList;
import java.util.List;

import fisher.syn.core.Syntax;

public  class  CompilerMessage  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public final DangerLevel danger;
	public final Syntax src;
	public final String shortText;
	public final String longText;
	public final Object[] details;
	public CompilerMessage(DangerLevel danger, Syntax src, String shortText, String text, Object[] details) {
		super();
		this.danger = danger;
		this.src = src;
		this.shortText = shortText;
		this.longText = text;
		this.details = details;
	}
	
	public String toString() {
		return 
			danger + ":  " + shortText + "\n"  
			+ longText + "\n"
			+ (src.source == null ?  "(generated)" : src.source.original(src))
			+ (Bard.sep(details, "\n"))
			;
	}
	
	public boolean matchesClue(String clue) {
		String string = this.toString();
		boolean matches = string.contains(clue);
		return matches;
	}
	

	
}
