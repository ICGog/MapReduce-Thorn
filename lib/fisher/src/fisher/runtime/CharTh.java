
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.runtime;

import java.lang.Character.UnicodeBlock;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static java.lang.Character.*;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.syn.core.Syntax;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  CharTh extends ThingImmutable  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	private final int codepoint; 
	
	private CharTh(int i) {
		this.codepoint = i;
	}
	
	public static CharTh of(int i, Syntax src) throws FisherException  {
		if (!(Character.isValidCodePoint(i))) {
			Doom.runtime("Not a codepoint: " + i, src);
		}
		return new CharTh(i);
	}
	
	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		// TODO Auto-generated method stub
		return true;
	}

	@Override
	public String typeString() {
		// TODO Auto-generated method stub
		return "char";
	}
	
	@Override
	public Object unthingify() {
		if (Character.isSupplementaryCodePoint(codepoint)) return new Integer(codepoint);
		else return new Character((char)codepoint);
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj == null) return false;
		if (obj.getClass() == CharTh.class) {
			CharTh that = (CharTh) obj;
			return this.codepoint == that.codepoint;			
		}
		else return false;
	}
	
	@Override
	public int hashCode() {
		return codepoint;
	}

	private final static int STR = 0;
	private final static int NUM = 1;
	private final static int Cdefined = 2;
	private final static int Ccategory = 3;
	private final static int CdirLR = 4;
	private final static int Cdirectionality = 5;
	private final static int Cdigit = 6;
	private final static int CidentifierIgnorable = 7;
	private final static int Ccontrol = 8;
	private final static int Cletter = 9;
	private final static int CletterOrDigit = 10;
	private final static int Clower = 11;
	private final static int Cmirrored = 12;
	private final static int Cwhitespace = 13;
	private final static int Ctitle = 14;
	private final static int CunicodeIdentifierPart = 15;
	private final static int CunicodeIdentifierStart = 16;
	private final static int Cupper = 17;
	private final static int CVlower = 18;
	private final static int CVupper = 19;
	private final static int CVtitle = 20;
	private final static int Cblock = 21;
	private final static int Cplus = 22;

	private static Map<String, Integer> methodCode = new HashMap<String, Integer>();
	static {
		methodCode.put("num", NUM);
		methodCode.put("len", NUM);
		methodCode.put("str", STR);
		methodCode.put("defined?", Cdefined);
		methodCode.put("category", Ccategory);
		methodCode.put("dirLR?", CdirLR);
		methodCode.put("directionality", Cdirectionality);
		methodCode.put("digit?", Cdigit);
		methodCode.put("identifierIgnorable?", CidentifierIgnorable);
		methodCode.put("control?", Ccontrol);
		methodCode.put("letter?", Cletter);
		methodCode.put("letterOrDigit?", CletterOrDigit);
		methodCode.put("lowercase?", Clower);
		methodCode.put("lowerCase?", Clower);
		methodCode.put("lower?", Clower);
		methodCode.put("mirrored?", Cmirrored);
		methodCode.put("whitespace?", Cwhitespace);
		methodCode.put("space?", Cwhitespace);
		methodCode.put("title?", Ctitle);
		methodCode.put("unicodeIdentifierPart?", CunicodeIdentifierPart);
		methodCode.put("unicodeIdentifierStart?", CunicodeIdentifierStart);
		methodCode.put("upper?", Cupper);
		methodCode.put("upperCase?", Cupper);
		methodCode.put("uppercase?", Cupper);
		methodCode.put("lower", CVlower);
		methodCode.put("lowercase", CVlower);
		methodCode.put("lowerCase", CVlower);
		methodCode.put("upper", CVupper);
		methodCode.put("uppercase", CVupper);
		methodCode.put("upperCase", CVupper);
		methodCode.put("title", CVtitle);
		methodCode.put("titlecase", CVtitle);
		methodCode.put("titleCase", CVtitle);
		methodCode.put("block", Cblock);
		methodCode.put("+", Cplus);
	}

	@Override
	public Thing invokeMethod(String methodName, Thing[] args,  Syntax src)
			throws FisherException {
		if (methodCode.containsKey(methodName)) {
			Integer methodC = methodCode.get(methodName);
			Evaller evaller = Evaller.mine();
			Framelike frame = this.frameForInvoke();
			switch (methodC) {
			case STR:
				return this.str();
			case NUM:
				return this.NUM(methodName, args, evaller, frame, src);
			case Cdefined:
				return this.Cdefined(methodName, args, evaller, frame, src);
			case Ccategory:
				return this.Ccategory(methodName, args, evaller, frame, src);
			case CdirLR:
				return this.CdirLR(methodName, args, evaller, frame, src);
			case Cdirectionality:
				return this.Cdirectionality(methodName, args, evaller, frame, src);
			case Cdigit:
				return this.Cdigit(methodName, args, evaller, frame, src);
			case CidentifierIgnorable:
				return this.CidentifierIgnorable(methodName, args, evaller, frame, src);
			case Ccontrol:
				return this.Ccontrol(methodName, args, evaller, frame, src);
			case Cletter:
				return this.Cletter(methodName, args, evaller, frame, src);
			case CletterOrDigit:
				return this.CletterOrDigit(methodName, args, evaller, frame, src);
			case Clower:
				return this.Clower(methodName, args, evaller, frame, src);
			case Cmirrored:
				return this.Cmirrored(methodName, args, evaller, frame, src);
			case Cwhitespace:
				return this.Cwhitespace(methodName, args, evaller, frame, src);
			case Ctitle:
				return this.Ctitle(methodName, args, evaller, frame, src);
			case CunicodeIdentifierPart:
				return this.CunicodeIdentifierPart(methodName, args, evaller, frame, src);
			case CunicodeIdentifierStart:
				return this.CunicodeIdentifierStart(methodName, args, evaller, frame, src);
			case Cupper:
				return this.Cupper(methodName, args, evaller, frame, src);
			case CVlower:
				return this.CVlower(methodName, args, evaller, frame, src);
			case CVupper:
				return this.CVupper(methodName, args, evaller, frame, src);
			case CVtitle:
				return this.CVtitle(methodName, args, evaller, frame, src);
			case Cblock:
				return this.Cblock(methodName, args, evaller, frame, src);
			case Cplus:
				return this.Cplus(methodName, args, evaller, frame, src);

			default:
				Doom.internal("Method key structure broken", src, methodC);
				return null;
			}
		}
		return this.tryThingMethods(methodName, args,   src);
	}
	
	
	private Thing Cplus(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		
		String s1 = this.toString();
		String s2 = EvalUtil.toString(args[0]);
		return StringTh.of(s1+s2);
	}
	
	private Thing Cblock(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		final UnicodeBlock block = Character.UnicodeBlock.of(codepoint);
		String blockname = block.toString();
		return StringTh.of(blockname);
	}
	private CharTh CVtitle(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		final int i = Character.toTitleCase(codepoint);
		return CharTh.of(i, src);
	}
	private CharTh CVupper(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		final int i = Character.toUpperCase(codepoint);
		return CharTh.of(i, src);
	}
	private CharTh CVlower(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		final int i = Character.toLowerCase(codepoint);
		return CharTh.of(i, src);
	}
	private BoolTh Cupper(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		final boolean b = Character.isUpperCase(codepoint);
		return BoolTh.of(b);
	}
	private BoolTh CunicodeIdentifierStart(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		final boolean b = Character.isUnicodeIdentifierStart(codepoint);
		return BoolTh.of(b);
	}
	private BoolTh CunicodeIdentifierPart(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		final boolean b = Character.isUnicodeIdentifierPart(codepoint);
		return BoolTh.of(b);
	}
	private BoolTh Ctitle(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		final boolean b = Character.isTitleCase(codepoint);
		return BoolTh.of(b);
	}
	private BoolTh Cwhitespace(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		final boolean b = Character.isWhitespace(codepoint);
		return BoolTh.of(b);
	}
	private BoolTh Cmirrored(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		final boolean b = Character.isMirrored(codepoint);
		return BoolTh.of(b);
	}
	private BoolTh Clower(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		final boolean b = Character.isLowerCase(codepoint);
		return BoolTh.of(b);
	}
	private BoolTh CletterOrDigit(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		final boolean b = Character.isLetterOrDigit(codepoint);
		return BoolTh.of(b);
	}
	private BoolTh Cletter(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		final boolean b = Character.isLetter(codepoint);
		return BoolTh.of(b);
	}
	private BoolTh Ccontrol(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return BoolTh.of(Character.isISOControl(codepoint));
	}
	private BoolTh CidentifierIgnorable(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return BoolTh.of(Character.isIdentifierIgnorable(codepoint));
	}
	
	private BoolTh Cdigit(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return BoolTh.of(Character.isDigit(codepoint));
	}
	
	public String toString() {
		char[] chars = Character.toChars(codepoint);
		String s = new String(chars);
//		if (codepoint > 0xFFFF) {
//			System.err.println("CharTh.toString confirming....");
//			System.err.println("length = " +chars.length);
//			System.err.println("s=" + s);
//			System.err.println("s.length()=" + s.length());
//			System.err.println("n codepoints in s = " + s.codePointCount(0, s.length()-1));
//		}
		return s;
	}
	private IntTh NUM(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return IntTh.of(this.codepoint);
	}
	private BoolTh CdirLR(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		String dirType = directionalityString(codepoint);
		Boolean RL = 
			"R".equals(dirType) 
			|| "AL".equals(dirType)
			|| "RLE".equals(dirType)
			|| "RLO".equals(dirType);
			
		return BoolTh.of(! RL);
	}
	private BoolTh Cdefined(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return BoolTh.of(Character.isDefined(codepoint));
	}
	
	private Thing Cdirectionality(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		final int n = Character.getDirectionality(codepoint);
		String s = directionalityString(n);
		return s == null ? null : StringTh.of(s);
	}

	private String directionalityString(final int n) {
		String s ;
		switch(n) {
		case DIRECTIONALITY_ARABIC_NUMBER : s = "AN"; break;
		case DIRECTIONALITY_BOUNDARY_NEUTRAL : s = "BN"; break;
		case DIRECTIONALITY_COMMON_NUMBER_SEPARATOR : s = "CS"; break;
		case DIRECTIONALITY_EUROPEAN_NUMBER : s = "EN"; break;
		case DIRECTIONALITY_EUROPEAN_NUMBER_SEPARATOR : s = "ES"; break;
		case DIRECTIONALITY_EUROPEAN_NUMBER_TERMINATOR : s = "ET"; break;
		case DIRECTIONALITY_LEFT_TO_RIGHT : s = "L"; break;
		case DIRECTIONALITY_LEFT_TO_RIGHT_EMBEDDING : s = "LRE"; break;
		case DIRECTIONALITY_LEFT_TO_RIGHT_OVERRIDE : s = "LRO"; break;
		case DIRECTIONALITY_NONSPACING_MARK : s = "NSM"; break;
		case DIRECTIONALITY_OTHER_NEUTRALS : s = "ON"; break;
		case DIRECTIONALITY_PARAGRAPH_SEPARATOR : s = "B"; break;
		case DIRECTIONALITY_POP_DIRECTIONAL_FORMAT : s = "PDF"; break;
		case DIRECTIONALITY_RIGHT_TO_LEFT : s = "R"; break;
		case DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC : s = "AL"; break;
		case DIRECTIONALITY_RIGHT_TO_LEFT_EMBEDDING : s = "RLE"; break;
		case DIRECTIONALITY_RIGHT_TO_LEFT_OVERRIDE : s = "RLO"; break;
		case DIRECTIONALITY_SEGMENT_SEPARATOR : s = "S"; break;
		case DIRECTIONALITY_UNDEFINED : s = null; break;
		case DIRECTIONALITY_WHITESPACE : s = "WS"; break;
		default: s = null;
		}
		return s;
	}
	
	
	
	
	
	private Thing Ccategory(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		final int n = Character.getType(codepoint);
		String s ;
		switch(n) {
		case COMBINING_SPACING_MARK : s = "Mc"; break;
		case CONNECTOR_PUNCTUATION : s = "Pc"; break;
		case CONTROL : s = "Cc"; break;
		case CURRENCY_SYMBOL : s = "Sc"; break;
		case DASH_PUNCTUATION : s = "Pd"; break;
		case DECIMAL_DIGIT_NUMBER : s = "Nd"; break;
		case ENCLOSING_MARK : s = "Me"; break;
		case END_PUNCTUATION : s = "Pe"; break;
		case FINAL_QUOTE_PUNCTUATION : s = "Pf"; break;
		case FORMAT : s = "Cf"; break;
		case INITIAL_QUOTE_PUNCTUATION : s = "Pi"; break;
		case LETTER_NUMBER : s = "Nl"; break;
		case LINE_SEPARATOR : s = "Zl"; break;
		case LOWERCASE_LETTER : s = "Ll"; break;
		case MATH_SYMBOL : s = "Sm"; break;
		case MODIFIER_LETTER : s = "Lm"; break;
		case MODIFIER_SYMBOL : s = "Sk"; break;
		case NON_SPACING_MARK : s = "Mn"; break;
		case OTHER_LETTER : s = "Lo"; break;
		case OTHER_NUMBER : s = "No"; break;
		case OTHER_PUNCTUATION : s = "Po"; break;
		case OTHER_SYMBOL : s = "So"; break;
		case PARAGRAPH_SEPARATOR : s = "Zp"; break;
		case PRIVATE_USE : s = "Co"; break;
		case SPACE_SEPARATOR : s = "Zs"; break;
		case START_PUNCTUATION : s = "Ps"; break;
		case SURROGATE : s = "Cs"; break;
		case TITLECASE_LETTER : s = "Lt"; break;
		case UNASSIGNED : s = "Cn"; break;
		case UPPERCASE_LETTER : s = "Lu"; break;
		default: s = null;
		}
		return StringTh.of(s);
	}
	

}
