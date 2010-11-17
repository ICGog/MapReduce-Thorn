
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

import java.util.ArrayList;
import java.util.List;

import fisher.eval.Evaller;
import fisher.parser.FisherParser;
import fisher.parser.ParseException;
import fisher.parser.Token;
import fisher.runtime.StringTh;
import fisher.syn.AbstractStringBit;
import fisher.syn.Cmd;
import fisher.syn.Literal;
import fisher.syn.StringBitText;
import fisher.syn.StringBitVar;
import fisher.syn.StringWithInterpolations;
import fisher.syn.VarExp;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;

public enum QuoteStyle {
	Q1("'"),
	Q2("\""),
	QQQ1("'''"),
	QQQ2("\"\"\"");
	
	public final String image;

	private QuoteStyle(String image) {
		this.image = image;
	}
	
	public String format(List<AbstractStringBit> bits) {
		StringBuffer sb = new StringBuffer();
		sb.append(this.image);
		for (AbstractStringBit bit : bits) {
			bit.formatted(this, sb);
		}
		sb.append(this.image);
		return sb.toString();
	}
	
	public static char[][] BACKSLASH_CONV = 
		new char[][] {
			new char[]{'\\', '\\'},
			new char[]{'"',  '"'},
			new char[]{'\'', '\''},
			new char[]{'\n', 'n'},
			new char[]{'\r', 'r'},
			new char[]{'\f', 'f'},
			new char[]{'\t', 't'},
			new char[]{'\b', 'b'},
			new char[]{'$',  '$'}
	};
	
	public StringTh deBackslash(Token t) throws FisherException{
		String s = t.image.substring(this.image.length(), t.image.length()-this.image.length());
		int i = 0;
		int len = s.length();
		StringBuffer sb = new StringBuffer();
		while (i < len) {
			char c = s.charAt(i);
			if (c == '\\') {
				if (i == len - 1) {
					Doom.runtime("A string literal can't have a backslash as its last character.", Evaller.mine().lastSyntax(), s);
				}
				char d = s.charAt(i+1);
				i += 1; // i now points at the char after \d
				boolean found = false;
				for(int j = 0; j < BACKSLASH_CONV.length; j++) {
					if (BACKSLASH_CONV[j][1] == d) {
						sb.append(BACKSLASH_CONV[j][0]);
						found = true;
						break;
					}
				}// for j
				if (!found) {
					Doom.runtime("Unknown backslash-escape in string: '" + d + "'", Evaller.mine().lastSyntax(), s);
				}
			}
			else {
				sb.append(c);
			}
			i += 1;
		}// while
		return StringTh.of(sb.toString());
	}
	
	public  Cmd engulf(Token t) throws ParseException {
		// t is the <STRING1> or whatever token.
		// Return a StringWithInterpolations which properly describes t.
		// s = the string to work from...
		String s = t.image.substring(this.image.length(), t.image.length()-this.image.length());
		int i = 0;
		int len = s.length();
		if (len == 0) return new Literal(t, t, "");
		StringBuffer sb = new StringBuffer();
		List<AbstractStringBit> bits = new ArrayList<AbstractStringBit>(1);
		while (i < len) {
			char c = s.charAt(i);
			if (c == '$') {
				putGatheredLiteralAway(t, sb, bits);				
				if (i == len - 1) {
					throw FisherParser.error(t, t, "String can't end with $");
				}
				char d = s.charAt(i+1);
				int j;
				String varname;
				boolean backquoted;
				if (d == '`') {
					j = scanToBackquote(t,s,i+2);
					varname = s.substring(i+2, j);
					i = j;
					backquoted = true;
				}
				else {
					j = scanIdentifier(t,s,i+1);
					varname = s.substring(i+1,j);
					i = j-1;
					backquoted = false;
				}
				putVarInterpolationAway(t, varname, backquoted, bits);
				
			}
			else if (c == '\\') {
				if (i == len - 1) {
					throw FisherParser.error(t, t, "String can't end with backslash");
				}
				char d = s.charAt(i+1);
				i += 1; // i now points at the char after \d
				boolean found = false;
				for(int j = 0; j < BACKSLASH_CONV.length; j++) {
					if (BACKSLASH_CONV[j][1] == d) {
						sb.append(BACKSLASH_CONV[j][0]);
						found = true;
						break;
					}
				}// for j
				if (!found) {
					throw FisherParser.error(t,t, "Unknown backslash-escaped character: \\" + d);
				}
			}
			else {
				sb.append(c);
			}
			i += 1;
		}// while
		putGatheredLiteralAway(t, sb, bits);
		if (bits.isEmpty()) {
			// Null string is common special case, so be nice to it.
			StringBitText sbt = new StringBitText(t,t,"");
			bits.add(sbt);
		} else /*Is it just a plain string, no interpolations? */{
			AbstractStringBit b0 = bits.get(0);
			
			if (bits.size() == 1 && b0 instanceof StringBitText) {
				String sConv = ((StringBitText)b0).value;
				return new Literal(t,t,sConv);
			}
		}
		return new StringWithInterpolations(t,t,bits, this);
	}
	
	private static int scanToBackquote(Token t, String s, int from) throws ParseException {
		int j = s.indexOf('`', from); 
		if (j < 0) {
			throw FisherParser.error(t, t, "Unclosed backquote at position " + from);
		}
		return j;
	}
	
	private static String id1 = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
	private static String id2 = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";
	
	private static int scanIdentifier(Token t, String s, int from) throws ParseException {
		assert id1.length() == 52;
		assert id2.length() == 63;
		char c1 = s.charAt(from);
		if (id1.indexOf(c1) < 0) {
			throw FisherParser.error(t,t,"The dollar-sign at position " + from + " needs an identifier after it.",
					"context: " + Bard.substring(s, from-5, from+5)
			);
		}
		int len = s.length();
		int j = from+1;
		while (j < len) {
			if (id2.indexOf(s.charAt(j)) < 0) {
				return j;
			}
			j ++;
		}
		return len;
	}
	
	private static void putVarInterpolationAway(Token t, String varname, boolean backquoted, List<AbstractStringBit> bits) {
		Id id = new Id(t, varname);
		VarExp var = new VarExp(t,t,id);
		StringBitVar v = new StringBitVar(t,t,var, backquoted);
		bits.add(v);
	}
	
	private static void putGatheredLiteralAway(Token t, StringBuffer sb, List<AbstractStringBit> bits) {
		String u = sb.toString();
		if (u.length() == 0) return;
		sb.delete(0, u.length());
		StringBitText sbt = new StringBitText(t,t,u);
		bits.add(sbt);		
	}
	
	
	public void formatString(String s, StringBuffer sb) {
		// Copy s to sb, but \-quote the chars which need it.
		for(int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			boolean cFound = false;
			for(int j = 0; j < BACKSLASH_CONV.length; j++) {
				if (BACKSLASH_CONV[j][0] == c) {
					cFound = true;
					sb.append('\\');
					sb.append(BACKSLASH_CONV[j][1]);
					break;
				}
			}
			if (!cFound) {
				sb.append(c);
			}
		}
	}
	
	public void formatVar(Cmd exp, boolean backquoted, StringBuffer sb) {
		sb.append("$");
		if (backquoted) sb.append("`");
		sb.append(exp.toString());
		if (backquoted) sb.append("`");
	}
}
