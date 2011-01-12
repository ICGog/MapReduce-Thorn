
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

import fisher.parser.FisherParser;
import fisher.parser.ParseException;
import fisher.parser.Token;

public  class  ParserAddenda  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	/*  I hate putting substantial code changes in machine-generated files.  This module is for changes to the parser.
	 */
	
	 /* This addendum is to produce parser errors that print some context. 
	 * 
	 * The code in ParseException to call this is: 
	 	public String toString() {
		return ParserAddenda.fisherException(this, super.toString());
     	}
	
	 */
	
	
	public static String fisherException(ParseException pex, String superToString) {
		Token tok = pex.currentToken;
		if (tok == null || tok.image == null) 
			return superToString;
		
		else 
			return 
			 "Parse error noticed at line " + tok.beginLine + ", column " + tok.beginColumn + ".  The code is:\n"
			+ FisherParser.current.source.betweenLinesOf(tok.beginLine-1, tok.endLine+1) + "\n"
			+ superToString
			;
	}
}
