
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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import fisher.syn.core.Syntax;

public  class  Compilation  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public static Compilation current = new Compilation();
	public static Set<Syntax> flagged = new HashSet<Syntax>();
	
	public List<CompilerMessage> messages = new ArrayList<CompilerMessage>();
	
	
	
	public static void flag(DangerLevel danger, Syntax src, String shortText, String text, Object... details){
		flag1(danger, src, shortText, text, details);
	}
	public static void flag1(DangerLevel danger, Syntax src, String shortText, String text, Object[] details){
		CompilerMessage message= new CompilerMessage(danger, src, shortText, text, details);
		flagged.add(src);
		Compilation.current.messages.add(message);
	}
	
	public void reset() {
		messages.clear();
		flagged.clear();
	}
	
	public static boolean isFlagged(Syntax syn) {
		return flagged.contains(syn);
	}
	
	
	
}
