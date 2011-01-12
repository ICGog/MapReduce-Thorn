
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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.StringReader;

import fisher.parser.FisherParser;
import fisher.parser.FisherParser2;
import fisher.parser.Token;
import fisher.runtime.builtInFun.PrintlnBIF;
import fisher.syn.core.Syntax;

public abstract  class  FisherSource  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	public String original(Syntax syn) {
		return
		"from " + this.sourceName() + ", from line " + syn.start.beginLine + " col " + syn.start.beginColumn + ""
		+ " to line " + syn.end.endLine + " col " + syn.end.endColumn + " \n"
		+ this.betweenLinesOf(syn.start, syn.end)
		;
	}
	
	public abstract boolean exists();
	
	public String betweenLinesOf(Token start, Token end){
		return this.betweenLinesOf(start.beginLine, end.endLine);
	}
	
	
	public String betweenLinesOf(int beginLine, int endLine){
		String s = this.string() + "\n";
		int nNL = 1;
		int pos = -1;
		if (beginLine < 0) beginLine = 0;
		int len = s.length();
		while (pos < len && nNL < beginLine) {
			pos = s.indexOf('\n', pos+1);
			if (pos == -1) {
				throw new RuntimeException("Oh, no! Math is broken! Or maybe the source is wrong!");
			}
			nNL += 1;
		}
		int startPos = pos+1;
		while (pos < len && nNL < endLine+1) {
			int npos = s.indexOf('\n', pos+1);
			if (npos == -1 && nNL != endLine) {
				throw new RuntimeException("Oh, no! Math is broken! Or maybe the source is wrong!");
			}
			else if (npos == -1 && nNL == endLine) {
				pos = len;
			}
			else {
				pos = npos;
			}
			nNL += 1;
		}
		int endPos = pos;
		return s.substring(startPos, endPos);
	}
	
	public abstract String string();
	public abstract String sourceName();
	
	public abstract FisherParser parser();
	public abstract FisherParser2 parser2();
	
	public static class FromString extends FisherSource{
		public final String string;

		public FromString(String string) {
			super();
			this.string = string;
		}
		
		@Override
		public String string() {
			return this.string;
		}
		
		@Override
		public String sourceName() {
			return "string";
		}
		
		@Override
		public boolean exists() {
			return true;
		}
		
		@Override
		public FisherParser parser() {
			// cf. ParserUtils.parseAt(s)
			StringReader sr = new StringReader(this.string());
			FisherParser parser = new FisherParser(sr);
			FisherParser.current = parser;
			parser.source = this;
			return parser;
		}
		@Override
		public FisherParser2 parser2() {
			// cf. ParserUtils.parseAt(s)
			StringReader sr = new StringReader(this.string());
			FisherParser2 parser = new FisherParser2(sr);
			FisherParser2.current = parser;
			parser.source = this;
			return parser;
		}
		
		@Override
		public String toString() {
			return string();
		}
		
	}
	
	public static class FromFile extends FisherSource{
		public final File file;
		public final String string;

		public FromFile(File file) {
			super();
			this.file = file;
			this.string = Bard.contentsOf(file);
//			System.out.println("from (" + file + ") we get ...\n" + string);
		}
		
		@Override
		public String string() {
			return this.string;
		}
		
		@Override
		public String sourceName() {
			return "file(" + file + ")"; 
		}
		
		@Override
		public boolean exists() {
			return file.exists();
		}
		
		@Override
		public FisherParser parser() {
			StringReader sr = new StringReader(this.string());
			FisherParser parser = new FisherParser(sr);
//				FileReader fr = new FileReader(file);
//				FisherParser parser = new FisherParser(fr);
			FisherParser.current = parser;
			parser.source = this;
			return parser;
		}
		
		@Override
		public FisherParser2 parser2() {
				StringReader sr = new StringReader(this.string());
				FisherParser2 parser2 = new FisherParser2(sr);
//				FileReader fr = new FileReader(file);
//				FisherParser parser = new FisherParser(fr);
				FisherParser2.current = parser2;
				parser2.source = this;
				return parser2;
		}
		
		@Override
		public String toString() {
			return file.toString();
		}
		
	}
	
	
}
