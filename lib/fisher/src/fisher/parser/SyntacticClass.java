
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.parser;

import java.util.ArrayList;
import java.util.List;

import fisher.syn.core.*;
import fisher.util.FisherSource;

public enum SyntacticClass implements SyntacticClassParses {
	STMT {
		public Syntax parse(FisherParser parser, FisherSource source) throws ParseException{
			return parser.TestStmt(source);
		}
		public Syntax parse2(FisherParser2 parser, FisherSource source) throws ParseException{
			return parser.TestStmt(source);
		}
	},
	STMTLIST {
		public Syntax parse(FisherParser parser, FisherSource source) throws ParseException{
			return parser.TestCmds(source);
		}
		public Syntax parse2(FisherParser2 parser, FisherSource source) throws ParseException{
			return parser.TestCmds(source);
		}
	},
	STMTS {
		public Syntax parse(FisherParser parser, FisherSource source) throws ParseException{
			return parser.SeqPossiblyOutsideBraces(source);
		}
		public Syntax parse2(FisherParser2 parser, FisherSource source) throws ParseException{
			return parser.SeqPossiblyOutsideBraces(source);
		}
	},
	EXP {
		public Syntax parse(FisherParser parser, FisherSource source) throws ParseException {
			return parser.TestExp(source);
		}
		public Syntax parse2(FisherParser2 parser, FisherSource source) throws ParseException {
			return parser.TestExp(source);
		}
	},
	PAT {
		public Syntax parse(FisherParser parser, FisherSource source) throws ParseException {
			return parser.TestPat(source);
		}
		public Syntax parse2(FisherParser2 parser, FisherSource source) throws ParseException {
			return parser.TestPat(source);
		}
	},
	CLS {
		public Syntax parse(FisherParser parser, FisherSource source) throws ParseException {
			return parser.TestClsDecl(source);
		}
		public Syntax parse2(FisherParser2 parser, FisherSource source) throws ParseException {
			return parser.TestClsDecl(source);
		}
	},
	MODU {
		public Syntax parse(FisherParser parser, FisherSource source) throws ParseException {
			return parser.TestModule(source);
		}
		public Syntax parse2(FisherParser2 parser, FisherSource source) throws ParseException {
			return parser.TestModule(source);
		}
	},
	MODUS {
		public Syntax parse(FisherParser parser, FisherSource source) throws ParseException {
			return parser.TestModules(source);
		}
		public Syntax parse2(FisherParser2 parser, FisherSource source) throws ParseException {
			return parser.TestModules(source);
		}
	},
	
	ID {
		public Syntax parse(FisherParser parser, FisherSource source) throws ParseException {
			return parser.TestId(source);
		}
		public Syntax parse2(FisherParser2 parser, FisherSource source) throws ParseException {
			return parser.TestId(source);
		}
	},
	
	SPAWN {
		public Syntax parse(FisherParser parser, FisherSource source) throws ParseException {
			return parser.SpawnFile(source);
		}
		public Syntax parse2(FisherParser2 parser, FisherSource source) throws ParseException {
			return parser.SpawnFile(source);
		}
	},
	SPAWN_OR_COMPONENT {
		public Syntax parse(FisherParser parser, FisherSource source) throws ParseException {
			return parser.SpawnOrComponentFile(source);
		}
		public Syntax parse2(FisherParser2 parser, FisherSource source) throws ParseException {
			return parser.SpawnOrComponentFile(source);
		}
	},
	ONE_SRC {
		public Syntax parse(FisherParser parser, FisherSource source) throws ParseException {
			return parser.ModulesAndCmds(source);
		}
		public Syntax parse2(FisherParser2 parser, FisherSource source) throws ParseException {
			return parser.ModulesAndCmds(source);
		}
	}
	;
	
	public abstract Syntax parse(FisherParser parser, FisherSource source) throws ParseException;
	public abstract Syntax parse2(FisherParser2 parser, FisherSource source) throws ParseException;
	
	
	public static List<SyntacticClass> orderedChoices = new ArrayList<SyntacticClass>();
	static {
		// Some choices need to go first 
		orderedChoices.add(MODUS);
		orderedChoices.add(STMTLIST);
		orderedChoices.add(STMTS);
		for (SyntacticClass sc : SyntacticClass.values()) {
			if (! orderedChoices.contains(sc)) orderedChoices.add(sc);
		}
	}
	public static Syntax parseAnyhowPossible(FisherParser parser, FisherSource source) {	
		for (SyntacticClass sc : orderedChoices) {
			try {
				final Syntax parsed = sc.parse(parser, source);
				return parsed;
			} catch (ParseException e) {
				continue;
			}
		}
		return null;		
	}
	
}
