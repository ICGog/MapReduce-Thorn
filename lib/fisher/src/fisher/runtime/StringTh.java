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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.management.RuntimeErrorException;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.*;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.runtime.auxil.String17BitTestedness;
import fisher.runtime.lib.json.JSON;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;
import fisher.util.FisherRuntimeException;
import static fisher.runtime.auxil.String17BitTestedness.*;

public class StringTh extends ThingImmutable implements fisher.runtime.Applyable {
	static String copyright() {
		return fisher.util.Copyright.IBM_COPYRIGHT;
	}

	/*
	 * If true, we turn on the stuff for handling 20-bit Unicode characters.  If not, 
	 * we stick with a Java-y interface.
	 */
	public static final boolean TwentyBitSafe = true;

	public static final StringTh EMPTY_STRING = new StringTh("");
	public String value;

	// UNTESTED: I don't know if this has 17-bit chars or not/
	// SIXTEENY: it does not have 17-bit chars;
	// SEVENTEENY: it does have 17-bit chars.
	private String17BitTestedness teenyness = TwentyBitSafe ? UNTESTED : SIXTEENY;

	private void teenynessTest() {
		if (!TwentyBitSafe)
			return;
		if (teenyness != UNTESTED)
			return;
		for (int i = 0; i < value.length(); i++) {
			char c = value.charAt(i);
			if (Character.MIN_SURROGATE <= c && c <= Character.MAX_SURROGATE) {
				teenyness = TWENTYY;
				//				System.out.println("StringTh.substring.TWENTYY - workaround of guessed 1.5 bug");
				//				StringBuffer sb = new StringBuffer();
				//				sb.append(value);
				//				value = sb.toString();
				return;
			}
		}
		teenyness = SIXTEENY;
		return;
	}

	private int cachedLength = -1;

	public static StringTh of(String s) {
		if (s == null)
			throw new NullPointerException();
		return new StringTh(s);
	}

	private StringTh(String value) {
		super();
		this.value = value;
	}

	public String toString() {
		return value;
	}

	@Override
	public boolean isString() {
		return true;
	}

	@Override
	public boolean transmissible() {
		return true;
	}

	@Override
	public Object unthingify() {
		return this.value;
	}

	public int length() {
		if (!TwentyBitSafe)
			return this.value.length();
		if (cachedLength >= 0)
			return cachedLength;
		teenynessTest();
		switch (teenyness) {
		case SIXTEENY:
			cachedLength = this.value.length();
			return cachedLength;
		case TWENTYY:
			cachedLength = this.value.codePointCount(0, this.value.length());
			return cachedLength;
		default:
			throw new RuntimeException("STringTh: teenyness is broken");
		}
	}

	public String substring(int i) {
		if (!TwentyBitSafe)
			return this.value.substring(i);
		teenynessTest();
		switch (teenyness) {
		case SIXTEENY:
			return this.value.substring(i);
		case TWENTYY:
			int ithCodePoint = this.value.offsetByCodePoints(0, i);
			int jthCodePoint = this.value.length();
			String s = this.value.substring(ithCodePoint);
			return s;
		default:
			throw new RuntimeException("STringTh: teenyness is broken");
		}
	}

	// Return this from pos i (inclusive) to pos j (exclusive).  
	// If j is off the right end of the string, trim to right end of string.
	public String substring(int i, int j) {

		if (j <= i)
			return "";
		if (j > this.length())
			j = this.length();
		String v = this.value;
		if (!TwentyBitSafe)
			return v.substring(i, j);
		teenynessTest();

		switch (teenyness) {
		case SIXTEENY:

			return v.substring(i, j);
		case TWENTYY:
			if (j < i)
				return "";

			int ithCodePoint = v.offsetByCodePoints(0, i);
			int endOfIthCodePoint = ithCodePoint + lengthOfCharacterStartingAt(ithCodePoint) - 1;
			// WAS: int justAfterJthCodePoint = this.value.offsetByCodePoints(ithCodePoint, j-i);
			int justAfterJthCodePoint = v.offsetByCodePoints(endOfIthCodePoint, j - i);
			//			int justAfterJthCodePoint = this.value.offsetByCodePoints(endOfIthCodePoint, j-i-1);
			// NOT THE PROBLEM: if (justAfterJthCodePoint > this.value.length()) justAfterJthCodePoint = this.value.length();
			String s = v.substring(ithCodePoint, justAfterJthCodePoint);
			return s;
		default:
			throw new RuntimeException("StringTh: teenyness is broken");
		}
	}

	public int lengthOfCharacterStartingAt(int k) {
		char c = value.charAt(k);
		if (Character.isLowSurrogate(c) || Character.isHighSurrogate(c))
			return 2;
		return 1;
	}

	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		return true;
	}

	@Override
	public String asString(Syntax src) throws FisherException {
		return value;
	}

	@Override
	public boolean equals(Object obj) {
		return obj != null && obj.getClass() == StringTh.class && ((StringTh) obj).value.equals(this.value);
	}

	@Override
	public int hashCode() {
		return value.hashCode();
	}

	@Override
	public String typeString() {
		return "string";
	}

	private final static int STR = 0;
	private final static int NUM = 1;
	private static final int COMPARETO = 2;
	private static final int COMPARETOIgnoringCase = 3;
	private static final int CcodePointAt = 4;
	private static final int Ccontains = 5;
	private static final int CendsWith = 6;
	private static final int CstartsWith = 7;
	private static final int CequalsIgnoreCase = 8;
	private static final int Creplace1 = 9;
	private static final int Creplace1RE = 10;
	private static final int Creplace = 11;
	private static final int CreplaceRE = 12;
	private static final int Csplit = 13;
	private static final int Csublen = 14;
	private static final int Ctrim = 15;
	private static final int Cindex = 17;
	private static final int CindexAfter = 18;
	private static final int Cint = 19;
	private static final int CintBin = 20;
	private static final int CintOct = 21;
	private static final int CintHex = 22;
	private static final int CintBase = 23;
	private static final int CmatchREp = 24;
	private static final int CmatchRE = 25;
	private static final int CmatchREAfter = 26;
	private static final int CmatchREAfterp = 27;
	private static final int CtoUpper = 28;
	private static final int CtoLower = 29;
	private static final int Ccapitalize = 30;
	private static final int Cfile = 31;
	private static final int Cdir = 32;
	private static final int ChashCode = 33;
	private static final int CmatchSlash = 34;
	private static final int Cplus = 35;
	private static final int Cjoin = 36;
	private static final int CPlusChar = 37;
	private static final int Cchar = 38;
	private static final int Csub = 39;
	private static final int Csubx = 40;
	private static final int Cleft = 41;
	private static final int Cbutleft = 42;
	private static final int Cright = 43;
	private static final int Cbutright = 44;
	private static final int Crange = 45;
	private static final int CwithSub = 46;
	private static final int CjavaChar = 47;
	private static final int CjavaLen = 48;
	private static final int CjavaPlus = 49;
	private static final int Cformat = 50;
	private static final int CwithSubx = 51;
	private static final int CwithSublen = 52;
	private static final int Crepeat = 53;
	private static final int Cquote = 54;
	private static final int Cfloat = 55;
	private static final int CdeJSON = 56;

	private static Map<String, Integer> methodCode = new HashMap<String, Integer>();
	static {
		methodCode.put("str", STR);
		methodCode.put("len", NUM);
		methodCode.put("num", NUM);
		methodCode.put("<=>", COMPARETO);
		methodCode.put("compareToIgnoreCase", COMPARETOIgnoringCase);
		methodCode.put("codePointAt", CcodePointAt);
		methodCode.put("contains?", Ccontains);
		methodCode.put("endsWith?", CendsWith);
		methodCode.put("startsWith?", CstartsWith);
		methodCode.put("equalsIgnoreCase?", CequalsIgnoreCase);
		methodCode.put("replace", Creplace);
		methodCode.put("replace1", Creplace1);
		methodCode.put("replaceRE", CreplaceRE);
		methodCode.put("replace1RE", Creplace1RE);
		methodCode.put("split", Csplit);
		methodCode.put("trim", Ctrim);
		methodCode.put("index", Cindex);
		methodCode.put("indexAfter", CindexAfter);
		methodCode.put("int", Cint);
		methodCode.put("intBase", CintBase);
		methodCode.put("intBin", CintBin);
		methodCode.put("intOct", CintOct);
		methodCode.put("intHex", CintHex);
		methodCode.put("matchRE?", CmatchREp);
		methodCode.put("matchRE", CmatchRE);
		methodCode.put("matchREAfter", CmatchREAfter);
		methodCode.put("matchREAfter?", CmatchREAfterp);
		methodCode.put("toUpper", CtoUpper);
		methodCode.put("toLower", CtoLower);
		methodCode.put("capitalize", Ccapitalize);
		methodCode.put("file", Cfile);
		methodCode.put("dir", Cdir);
		methodCode.put("hashCode", ChashCode);
		methodCode.put("match/", CmatchSlash);
		methodCode.put("+", Cplus);
		methodCode.put("join", Cjoin);
		methodCode.put("plusChar", CPlusChar);
		methodCode.put("char", Cchar);
		methodCode.put("sub", Csub);
		methodCode.put("subx", Csubx);
		methodCode.put("left", Cleft);
		methodCode.put("butleft", Cbutleft);
		methodCode.put("right", Cright);
		methodCode.put("butright", Cbutright);
		methodCode.put("sublen", Csublen);
		methodCode.put("range", Crange);
		methodCode.put("withSub", CwithSub);
		methodCode.put("javaChar", CjavaChar);
		methodCode.put("javaLen", CjavaLen);
		methodCode.put("javaPlus", CjavaPlus);
		methodCode.put("format", Cformat);
		methodCode.put("withSubx", CwithSubx);
		methodCode.put("withSublen", CwithSublen);
		methodCode.put("*", Crepeat);
		methodCode.put("quote", Cquote);
		methodCode.put("float", Cfloat);
		methodCode.put("deJSON", CdeJSON);

	}

	@Override
	public Thing invokeMethod(String methodName, Thing[] args, Syntax src) throws FisherException {
		if (methodCode.containsKey(methodName)) {
			Integer methodC = methodCode.get(methodName);
			Evaller evaller = Evaller.mine();
			Framelike frame = this.frameForInvoke();
			switch (methodC) {
			case STR:
				return this.str();
			case NUM:
				return this.num();
			case COMPARETO:
				return this.McompareTo(methodName, args, evaller, frame, src);
			case COMPARETOIgnoringCase:
				return this.McompareToIgnoreCase(methodName, args, evaller, frame, src);
			case CcodePointAt:
				return this.CcodePointAt(methodName, args, evaller, frame, src);
			case Ccontains:
				return this.Mcontains(methodName, args, evaller, frame, src);
			case CendsWith:
				return this.MendsWith(methodName, args, evaller, frame, src);
			case CstartsWith:
				return this.MstartsWith(methodName, args, evaller, frame, src);
			case CequalsIgnoreCase:
				return this.MequalsIgnoreCase(methodName, args, evaller, frame, src);
			case Creplace:
				return this.Mreplace(methodName, args, evaller, frame, src);
			case Creplace1:
				return this.Mreplace1(methodName, args, evaller, frame, src);
			case CreplaceRE:
				return this.MreplaceRE(methodName, args, evaller, frame, src);
			case Creplace1RE:
				return this.Mreplace1RE(methodName, args, evaller, frame, src);
			case Csplit:
				return this.Msplit(methodName, args, evaller, frame, src);
			case Ctrim:
				return this.trim();
			case Cindex:
				return this.Cindex(methodName, args, evaller, frame, src);
			case CindexAfter:
				return this.CindexAfter(methodName, args, evaller, frame, src);
			case Cint:
				return this.Cint(10, methodName, args, evaller, frame, src);
			case CintBase:
				return this.CintBase(methodName, args, evaller, frame, src);
			case CintOct:
				return this.Cint(8, methodName, args, evaller, frame, src);
			case CintHex:
				return this.Cint(16, methodName, args, evaller, frame, src);
			case CintBin:
				return this.Cint(2, methodName, args, evaller, frame, src);
			case CmatchREp:
				return this.CmatchREp(methodName, args, evaller, frame, src);
			case CmatchRE:
				return this.CmatchRE(methodName, args, evaller, frame, src);
			case CmatchREAfter:
				return this.CmatchREAfter(methodName, args, evaller, frame, src);
			case CmatchREAfterp:
				return this.CmatchREAfterp(methodName, args, evaller, frame, src);
			case CtoUpper:
				return this.CtoUpper(methodName, args, evaller, frame, src);
			case CtoLower:
				return this.CtoLower(methodName, args, evaller, frame, src);
			case Ccapitalize:
				return this.Ccapitalize(methodName, args, evaller, frame, src);
			case Cfile:
				return this.Cfile(methodName, args, evaller, frame, src);
			case Cdir:
				return this.Cdir(methodName, args, evaller, frame, src);
			case ChashCode:
				return this.ChashCode(methodName, args, evaller, frame, src);
			case CmatchSlash:
				return this.CmatchSlash(methodName, args, evaller, frame, src);
			case Cplus:
				return this.Cplus(methodName, args, evaller, frame, src);
			case Cjoin:
				return this.Cjoin(methodName, args, evaller, frame, src);
			case CPlusChar:
				return this.CplusChar(methodName, args, evaller, frame, src);
			case Cchar:
				return this.Cchar(methodName, args, evaller, frame, src);
			case Csub:
				return this.Csub(methodName, args, evaller, frame, src);
			case Csubx:
				return this.Csubx(methodName, args, evaller, frame, src);
			case Cleft:
				return this.Cleft(methodName, args, evaller, frame, src);
			case Cbutleft:
				return this.Cbutleft(methodName, args, evaller, frame, src);
			case Cright:
				return this.Cright(methodName, args, evaller, frame, src);
			case Cbutright:
				return this.Cbutright(methodName, args, evaller, frame, src);
			case Csublen:
				return this.Csublen(methodName, args, evaller, frame, src);
			case Crange:
				return this.Crange(methodName, args, evaller, frame, src);
			case CwithSub:
				return this.CwithSub(methodName, args, evaller, frame, src);
			case CjavaChar:
				return this.CjavaChar(methodName, args, evaller, frame, src);
			case CjavaLen:
				return this.CjavaLen(methodName, args, evaller, frame, src);
			case CjavaPlus:
				return this.CjavaPlus(methodName, args, evaller, frame, src);
			case Cformat:
				return this.Cformat(methodName, args, evaller, frame, src);
			case CwithSubx:
				return this.CwithSubx(methodName, args, evaller, frame, src);
			case CwithSublen:
				return this.CwithSublen(methodName, args, evaller, frame, src);
			case Crepeat:
				return this.Crepeat(methodName, args, evaller, frame, src);
			case Cquote:
				return this.Cquote(methodName, args, evaller, frame, src);
			case Cfloat:
				return this.Cfloat(methodName, args, evaller, frame, src);
			case CdeJSON:
				return this.CdeJSON(methodName, args, evaller, frame, src);
			default:
				Doom.internal("Method key structure broken: " + methodC, src, methodC);
				return null;
			}
		}
		return this.tryThingMethods(methodName, args, src);
	}

	private IntTh num() {
		return IntTh.of(this.length());
	}

	private StringTh trim() {
		return StringTh.of(this.value.trim());
	}

	private static Object unthing(Thing x) {
		return EvalUtil.unthingify(x, false);
	}

	private Thing Cformat(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		// NO -- checkNumberOfArgs(0,0, methodName, args, evaller, frame, src);
		switch (args.length) {
		case 0:
			return StringTh.of(String.format(value));
		case 1:
			return StringTh.of(String.format(value, unthing(args[0])));
		case 2:
			return StringTh.of(String.format(value, unthing(args[0]), unthing(args[1])));
		case 3:
			return StringTh.of(String.format(value, unthing(args[0]), unthing(args[1]), unthing(args[2])));
		case 4:
			return StringTh.of(String.format(value, unthing(args[0]), unthing(args[1]), unthing(args[2]),
					unthing(args[3])));
		default:
			return StringTh.of(String.format(value, EvalUtil.unthingifyAll(args, false)));
		}
	}

	private Thing Crepeat(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		int n = args[0].asJavaInt(src);
		if (n <= 0)
			return StringTh.EMPTY_STRING;
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < n; i++)
			sb.append(this.value);
		return StringTh.of(sb.toString());
	}

	private Thing Cquote(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		StringBuffer sb = new StringBuffer();
		sb.append("\"");
		for (int i = 0; i < value.length(); i++) {
			char c = value.charAt(i);
			if (c == '\'' || c == '\\' || c == '\"')
				sb.append("\\");
			switch (c) {
			case '\n':
				sb.append("\\n");
				break;
			case '\r':
				sb.append("\\r");
				break;
			default:
				sb.append(c);
				break;
			}
		}
		sb.append("\"");
		return StringTh.of(sb.toString());
	}

	private Thing CjavaLen(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return IntTh.of(this.value.length());
	}

	private Thing CjavaPlus(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		ListTh L = args[0].asList(src);
		char[] cs = new char[L.length];
		int n = L.length;
		for (int i = 0; i < n; i++) {
			Thing Li = L.car;
			L = L.cdr;
			int ci = Li.asJavaInt(src);
			if (ci < 0 || ci > Character.MAX_VALUE) {
				Doom.runtime("Not a javaChar: " + ci, src);
			}
			cs[i] = (char) ci;
		}
		String s = new String(cs);
		return StringTh.of(this.value + s);
	}

	private Thing CjavaChar(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		int i = args[0].asJavaInt(src);
		int pi = pos(i);
		if (pi < 0 || pi > this.value.length()) {
			Doom.runtime("Index out of bounds: " + pi + " into a string with javaLen=" + this.value.length(), src,
					"original index = " + i);
		}
		char c = this.value.charAt(i);
		return IntTh.of(c);
	}

	private Thing CwithSub(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(3, 3, methodName, args, evaller, frame, src);
		int i = args[0].asJavaInt(src);
		int pi = pos(i);
		int j = args[1].asJavaInt(src);
		int pj = pos(j);
		// SUB: String ss = substring(pi, pj+1);
		String before = substring(0, pi);
		String after = substring(pj + 1, length());
		String mid = args[2].asString(src);
		String s = before + mid + after;
		return StringTh.of(s);

	}

	private Thing CwithSubx(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(3, 3, methodName, args, evaller, frame, src);
		int i = args[0].asJavaInt(src);
		int pi = pos(i);
		int j = args[1].asJavaInt(src);
		int pj = pos(j);
		String before = substring(0, pi);
		String after = substring(pj, length());
		String mid = args[2].asString(src);
		String s = before + mid + after;
		return StringTh.of(s);

	}

	private Thing CwithSublen(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(3, 3, methodName, args, evaller, frame, src);
		int i = args[0].asJavaInt(src);
		int pi = pos(i);
		int n = args[1].asJavaInt(src);
		String before = substring(0, pi);
		int pj = pi + n;
		if (pj > this.length())
			pj = this.length();
		String after = substring(pj, length());
		String mid = args[2].asString(src);
		String s = before + mid + after;
		return StringTh.of(s);

	}

	private Thing Crange(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return IntRangeTh.of(0, this.length() - 1);
	}

	private Thing Cbutright(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		int i = args[0].asJavaInt(src);
		if (i < 0)
			Doom.runtime("Can't take the last " + i + " characters of a string.", src);
		final int n = this.length();
		if (i > n)
			i = n;
		String ss = substring(0, n - i);
		return StringTh.of(ss);
	}

	private Thing Cright(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		int i = args[0].asJavaInt(src);
		if (i < 0)
			Doom.runtime("Can't take the last " + i + " characters of a string.", src);
		final int n = this.length();
		if (i > n)
			i = n;
		String ss = substring(n - i, n);
		return StringTh.of(ss);
	}

	private Thing Cbutleft(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		int i = args[0].asJavaInt(src);
		if (i < 0)
			Doom.runtime("Can't take everything but the first " + i + " characters of a string.", src);
		String ss = substring(i, this.length());
		return StringTh.of(ss);
	}

	private Thing Cleft(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		int i = args[0].asJavaInt(src);
		if (i < 0)
			Doom.runtime("Can't take the first " + i + " characters of a string.", src);
		String ss = substring(0, i);
		return StringTh.of(ss);
	}

	private Thing Csubx(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		int i = args[0].asJavaInt(src);
		int pi = pos(i);
		int j = args[1].asJavaInt(src);
		int pj = pos(j);
		String ss = substring(pi, pj);
		return StringTh.of(ss);
	}

	private Thing Csub(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		int i = args[0].asJavaInt(src);
		int pi = pos(i);
		int j = args[1].asJavaInt(src);
		int pj = pos(j);
		String ss = substring(pi, pj + 1);
		return StringTh.of(ss);
	}

	private Thing Cchar(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		// TODO - 17-bit-ify
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		int i = args[0].asJavaInt(src);
		if (i < 0 || i >= this.length()) {
			return null;
		}
		int j = this.value.offsetByCodePoints(0, i);
		int cp = this.value.codePointAt(j);
		return CharTh.of(cp, src);
	}

	private Thing Cjoin(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		String joined = Bard.sep(EvalUtil.iter(args[0], src), this.value);
		return StringTh.of(joined);
	}

	private DirTh Cdir(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return DirTh.of(value, src);
	}

	private StringTh CplusChar(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		int i = args[0].asJavaInt(src);
		char c = (char) i;
		return StringTh.of(this.value + c);
	}

	private FileTh Cfile(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return FileTh.of(value);
	}

	private StringTh CtoUpper(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return StringTh.of(value.toUpperCase());
	}

	private StringTh CtoLower(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return StringTh.of(value.toLowerCase());
	}

	private StringTh Ccapitalize(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		String c;
		if (this.length() == 0) {
			c = "";
		} else {
			final String firstLetter = this.substring(0, 1);

			final String rest = this.substring(1);
			c = firstLetter.toUpperCase() + rest.toLowerCase();
		}
		return StringTh.of(c);
	}

	private StringTh Cplus(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		String re = EvalUtil.toString(args[0]);
		return StringTh.of(value + re);
	}

	private BoolTh CmatchREp(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		String re = args[0].asString(src);
		boolean b = value.matches(re);
		return BoolTh.of(b);
	}

	private BoolTh CmatchREAfterp(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(2, 3, methodName, args, evaller, frame, src);
		String re = args[0].asString(src);
		int pos = pos(args[1].asJavaInt(src));
		Pattern p = Pattern.compile(re);
		Matcher m = p.matcher(value);
		m.region(pos, value.length());
		boolean b = m.matches();
		return BoolTh.of(b);
	}

	private Thing CmatchREAfter(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		String re = args[0].asString(src);
		int pos = pos(args[1].asJavaInt(src));
		// TODO -- if Pattern doesn't cache, we could.
		Pattern p = Pattern.compile(re);
		// TODO - 17bitify
		Matcher m = p.matcher(value);
		m.region(pos, value.length());
		if (m.matches()) {
			return thornifyMatcher(src, m);
		} else {
			return null;
		}
	}

	private Thing CmatchSlash(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		String re = args[0].asString(src);
		// TODO -- if Pattern doesn't cache, we could.
		Pattern p = Pattern.compile(re);
		Matcher m = p.matcher(value);
		if (m.matches()) {
			return thornifyMatcherForSlash(src, m);
		} else {
			return null;
		}
	}

	private Thing CmatchRE(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		String re = args[0].asString(src);
		// TODO -- if Pattern doesn't cache, we could.
		Pattern p = Pattern.compile(re);
		Matcher m = p.matcher(value);
		if (m.matches()) {
			return thornifyMatcher(src, m);
		} else {
			return null;
		}
	}

	public static String[] matcherFields = new String[] { "start", "end", "text" };

	public static Thing thornifyMatcher(Syntax src, MatchResult m) throws FisherException {
		List<Thing> L = new ArrayList<Thing>();
		for (int g = 0; g <= m.groupCount(); g++) {
			int start = m.start(g);
			int end = m.end(g);
			String group = m.group(g);
			RecordTh rg = RecordTh.make(src, matcherFields, IntTh.of(start), IntTh.of(end), StringTh.of(group));
			L.add(rg);
		}
		return ListTh.fromJavaList(L);
	}

	public static Thing thornifyMatcherForSlash(Syntax src, MatchResult m) throws FisherException {
		List<Thing> L = new ArrayList<Thing>();
		for (int g = 1; g <= m.groupCount(); g++) {
			String group = m.group(g);
			L.add(StringTh.of(group));
		}
		return ListTh.fromJavaList(L);
	}

	private IntTh CintBase(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		int radix = (int) args[0].asLong(src);
		try {
			long l = Long.parseLong(value, radix);
			return IntTh.of(l);
		} catch (NumberFormatException e) {
			return null;
		}
	}

	private Thing CdeJSON(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		Thing dejsoned = JSON.parse(this);
		return dejsoned;
	}

	private FloatTh Cfloat(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		try {
			Double l = Double.parseDouble(value);
			return FloatTh.of(l);
		} catch (NumberFormatException e) {
			return null;
		}
	}

	private IntTh Cint(int radix, String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		try {
			long l = Long.parseLong(value, radix);
			return IntTh.of(l);
		} catch (NumberFormatException e) {
			return null;
		}
	}

	private StringTh Mreplace(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		String target = args[0].toString();
		String replacement = args[1].toString();
		return StringTh.of(this.value.replace(target, replacement));
	}

	private StringTh Mreplace1(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		String target = args[0].toString();
		String replacement = args[1].toString();
		Doom.notYet("Sorry, didn't get aroudn to replace1.");
		return null;
	}

	private StringTh MreplaceRE(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		String regexp = args[0].toString();
		String replacement = args[1].toString();
		return StringTh.of(this.value.replaceAll(regexp, replacement));
	}

	private StringTh Mreplace1RE(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		String regexp = args[0].toString();
		String replacement = args[1].toString();
		return StringTh.of(this.value.replaceFirst(regexp, replacement));
	}

	private IntTh CindexAfter(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		String str = args[0].toString();
		int start = args[1].asJavaInt(src);
		int i = value.indexOf(str, start);
		if (i < 0)
			return null;
		return IntTh.of(i);
	}

	private IntTh Cindex(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		String str = args[0].toString();
		int i = value.indexOf(str);
		if (i < 0)
			return null;
		return IntTh.of(i);
	}

	private Thing Msplit(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		String regexp = args[0].toString();
		String[] splut = value.split(regexp);
		return EvalUtil.thingify(splut, src);
	}

	private IntTh McompareTo(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		String other = args[0].toString();
		return IntTh.of(value.compareTo(other));
	}

	private StringTh Csublen(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		int startMaybeNeg = (int) args[0].asLong(src);
		int start = pos(startMaybeNeg);
		int length = (int) args[1].asLong(src);
		int end = start + length;
		if (end > value.length())
			end = value.length();
		String ss = this.substring(start, end);
		return StringTh.of(ss);
	}

	private IntTh McompareToIgnoreCase(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		String other = args[0].toString();
		return IntTh.of(value.compareToIgnoreCase(other));
	}

	private IntTh CcodePointAt(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		int i = args[0].asJavaInt(src);
		int cp = this.value.codePointAt(i);
		return IntTh.of(cp);
	}

	private BoolTh Mcontains(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		String s = args[0].toString();
		boolean cp = this.value.contains(s);
		return BoolTh.of(cp);
	}

	private BoolTh MendsWith(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		String s = args[0].toString();
		boolean cp = this.value.endsWith(s);
		return BoolTh.of(cp);
	}

	private BoolTh MstartsWith(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		String s = args[0].toString();
		boolean cp = this.value.startsWith(s);
		return BoolTh.of(cp);
	}

	private BoolTh MequalsIgnoreCase(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		String s = args[0].toString();
		boolean cp = this.value.equalsIgnoreCase(s);
		return BoolTh.of(cp);
	}

	public int pos(long i) {
		int len = this.length();
		if (i > len)
			return len;
		if (i >= 0)
			return (int) i;
		if (i >= -len)
			return (int) len + (int) i;
		return 0;
	}

	public int posForEnd(long i) {
		int lenp1 = this.length() + 1;
		if (i > lenp1)
			return lenp1;
		if (i >= 0)
			return (int) i;
		if (i >= -lenp1)
			return (int) lenp1 + (int) i;
		return 0;
	}

	public Thing apply(Thing[] args, Framelike frame, Evaller evaller, Syntax src) throws FisherException {
		int len = this.length();
		if (args.length == 1 && args[0].isNumber()) {
			long i = args[0].asLong(src);
			int pi = pos(i);
			if (pi >= len) {
				Doom.runtime("String index out of bounds: " + i + " on string of length " + len, src, this, i);
			}
			final String substring = this.substring(pi, pi + 1);
			String sub = substring;
			return StringTh.of(sub);
		} else if (args.length == 1 && args[0].isRange()) {
			IntRangeTh r = args[0].asIntRange(src);
			int i = (int) r.min;
			int j = (int) r.max;
			String sub = this.substring(i, j + 1);
			return StringTh.of(sub);
		}
		/*else if (args.length == 2) {
			long i = args[0].asLong(src);
			int pi = pos(i);
			if (pi > len) {
				pi = len;
			}
			long j = args[1].asLong(src);
			int pj = posForEnd(j);
			if (pi > len) {
				Doom.runtime("String index out of bounds: " + j + " on string of length " + len, src, this, i, pj);
			}
			String sub;
			if (pi == len || pj <= pi) {
				return StringTh.EMPTY_STRING;
			} else if (pj < len) {
				sub = this.substring(pi, pj);
			} else {
				sub = this.substring(pi);
			}
			return StringTh.of(sub);
		} */else {
			Doom.runtime("Wrong number of subscripts for string; it can take only 1.", src, this, args);
			return null;
		}
	}

	//	public static final int Pindex = 1;
	//	public static final int PindexAfter = 2;
	//	public static final Map<String, Integer> patCode = new HashMap<String, Integer>();
	//	static {
	//		patCode.put("index", Pindex);
	//		patCode.put("indexAfter", PindexAfter);
	//	}
	//
	//	public Internal_Success invokePat(Id patId, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	//			throws FisherException {
	//		String patName = patId.str();
	//		if (patCode.containsKey(patName)) {
	//			Integer patC = patCode.get(patName);
	//			switch (patC) {
	//			case Pindex:
	//				return Eindex(patId, args, evaller, frame, src);
	//			case PindexAfter:
	//				return EindexAfter(patId, args, evaller, frame, src);
	//			default:
	//				Doom.internal("Method key structure broken", src, patC);
	//				return null;
	//			}
	//		} else {
	//			return patNotUnderstood(patId, args, evaller, frame, src);
	//		}
	//	}
	//
	//	private Internal_Success Eindex(Id patId, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	//			throws FisherException {
	//		checkNumberOfArgs(1, 1, patId.str(), args, evaller, frame, src);
	//		String s = args[0].asString(src);
	//		int i = value.indexOf(s);
	//		if (i >= 0)
	//			return Internal_Success.of(IntTh.of(i));
	//		else
	//			return null;
	//	}
	//
	//	private Internal_Success EindexAfter(Id patId, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	//			throws FisherException {
	//		checkNumberOfArgs(2, 2, patId.str(), args, evaller, frame, src);
	//		String s = args[0].asString(src);
	//		int p = (int) args[1].asLong(src);
	//		int i = value.indexOf(s,p);
	//		if (i >= 0)
	//			return Internal_Success.of(IntTh.of(i));
	//		else
	//			return null;
	//	}

}
