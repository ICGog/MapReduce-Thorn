
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

import java.io.File;
import java.io.StringReader;
import java.util.List;
import java.util.Set;

import junit.framework.Assert;

import fisher.syn.chart.SynPtr;
import fisher.syn.core.*;
import fisher.util.Bard;
import fisher.util.FisherException;
import fisher.util.FisherSource;
import fisher.util.FisherSource.FromString;
import fisher.parser.*;
import static fisher.parser.SyntacticClass.*;

public  class  ParserUtils  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	public static Syntax parse(String loc, String s, SyntacticClass cls) throws ParseException {
		FisherParser parser = ParserUtils.parseAt(s);
		try {
			Syntax syn = cls.parse(parser, new FisherSource.FromString(s));
			syn.fillInParentPtrs();
			return syn;
		} catch (ParseException e) {
			throw new ParseException(loc + "/parse \n\n" +   e);			
		} catch(TokenMgrError e) {
			throw new ParseException(loc + "/parse(token) \n\n" +   e);	
		} catch(FisherException fe) {
			throw new ParseException(loc = "/parse(fisherEx)\n" + fe);
		}
		
	}
	
	public static Syntax parse(String loc, File f, SyntacticClass cls) throws ParseException {
		String s = Bard.contentsOf(f);
		System.err.println("Oh, dingoe. " + s ); 
		Bard.printNonASCIIChars(s);
		FisherParser parser = ParserUtils.parseAt(s);
		try {
			Syntax syn = cls.parse(parser, new FisherSource.FromFile(f));
			syn.fillInParentPtrs();
			return syn;
		} catch (ParseException e) {
			throw new ParseException(loc + "/parse \n\n" +   e);			
		} catch(TokenMgrError e) {
			throw new ParseException(loc + "/parse(token) \n\n" +   e);	
		} catch(FisherException fe) {
			throw new ParseException(loc = "/parse(fisherEx)\n" + fe);
		}
		
	}

	public static FisherParser parseAt(String s) {
		// String finis = FisherParserConstants.tokenImage[FisherParserConstants.FINIS]; 
		String sf = s ; // + " " + finis.substring(1, finis.length()-1);
		StringReader sr = new StringReader(sf);
		FisherParser parser = new FisherParser(sr);
		FisherParser.current = parser;
		parser.source = new FisherSource.FromString(sf);
		return parser;
	}
	

}
