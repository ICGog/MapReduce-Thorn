
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

import java.util.*;

import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import fisher.parser.ParseException;
import fisher.parser.ParserUtils;
import fisher.parser.SyntacticClass;
import fisher.parser.Token;
import fisher.syn.core.*;
import fisher.syn.*;
import fisher.test.TestUtils;
import fisher.util.Doom;
import fisher.util.FisherException;
import fisher.util.FisherInternalCompilerDoom;

public  class  DesugarUtils  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	private DesugarUtils(){}

	
	public static char GENSYM = '\u00A9';
	public static boolean NumberGensyms = true; // May be set to false for testing purposes.
	
	private static long gensymCounter = 0;
	
	public static Id gensym(Token locator, String clue) {
		String name = "" + GENSYM + (NumberGensyms ? "" + gensymCounter : "") + clue + GENSYM; 
		gensymCounter += 1;
		return new Id(locator, name, true);
	}
	
	public static List<Id> gensymEach(Token locator, List<? extends Object> clues, String spareClue) {
		List<Id> L = new ArrayList<Id>(clues.size());
		for (Object c : clues) {
			Id id = gensym(locator, c.toString() + spareClue);
			L.add(id);
		}
		return L;
	}
	
	public static List<Cmd> deepCopyCmdsRenaming(Token start, Token end, List<Cmd> cmds, Map<String, Id> mutations) {
		List L = new ArrayList<Cmd>(cmds.size());
		for (Cmd c : cmds) {
			Cmd c2 = deepCopyCmdRenaming(start, end, c, mutations);
			L.add(c2);
		}
		return L;
	}
	
	public static Cmd deepCopyCmdRenaming(Token start, Token end, Cmd cmd, Map<String, Id> mutations) {
		if (cmd instanceof VarExp) {
			VarExp ve = (VarExp) cmd;
			if (mutations.containsKey(ve.id.str())) {
				return new VarExp(ve.start, ve.end, (Id) mutations.get(ve.id.str()).ingestedDeepCopy(ve.start, ve.end));
			}
			else {
				return (VarExp) ve.ingestedDeepCopy(ve.start, ve.end);
			}
		}
		else if (cmd instanceof Literal) {
			Literal lit = (Literal) cmd;
			return (Literal) lit.ingestedDeepCopy(lit.start, lit.end);
		}
		else {
			Doom.notYet();
			return null;
		}
	}
	
}
