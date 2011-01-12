
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.runtime.dunno;

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
import fisher.runtime.IntTh;
import fisher.runtime.Thing;
import fisher.runtime.ThingImmutable;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.runtime.auxil.String17BitTestedness;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;
import fisher.util.FisherRuntimeException;
import static fisher.runtime.auxil.String17BitTestedness.*;

public  class  StringTh_17 extends ThingImmutable implements fisher.runtime.Applyable  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	public static final boolean SeventeenBitSafe = false;

	public static final StringTh_17 EMPTY_STRING = new StringTh_17("");
	public String value;
	
	// UNTESTED: I don't know if this has 17-bit chars or not/
	// SIXTEENY: it does not have 17-bit chars;
	// SEVENTEENY: it does have 17-bit chars.
	private String17BitTestedness teenyness = SeventeenBitSafe ? UNTESTED : SIXTEENY;
	
	private void teenynessTest() {
		if (!SeventeenBitSafe) return;
		if (teenyness != UNTESTED) return;
		for(int i = 0; i < value.length(); i++) {
			char c = value.charAt(i);
			if (Character.MIN_SURROGATE <= c && c <= Character.MAX_SURROGATE) {
				teenyness = TWENTYY;
				return;
			}
		}
		teenyness = SIXTEENY;
		return;
	}
	
	private int cachedLength = -1;

	public static StringTh_17 of(String s) {
		if (s == null)
			throw new NullPointerException();
		return new StringTh_17(s);
	}

	private StringTh_17(String value) {
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

	public int length() {
		if (!SeventeenBitSafe) return this.value.length();
		if (cachedLength >= 0) return cachedLength;
		teenynessTest();
		switch(teenyness) {
		case SIXTEENY: cachedLength =  this.value.length();
			return cachedLength;
		case TWENTYY: cachedLength = this.value.codePointCount(0, this.value.length()-1);
			return cachedLength;
		default: throw new RuntimeException("STringTh: teenyness is broken");
		}
	}
	
	
	public String substring(int i) {
		if (!SeventeenBitSafe) return this.value.substring(i);
		teenynessTest();
		switch(teenyness) {
		case SIXTEENY: 
			return this.value.substring(i);
		case TWENTYY:
			int ithCodePoint = this.value.offsetByCodePoints(0, i);
			int jthCodePoint = this.value.length();
			String s = this.value.substring(ithCodePoint, jthCodePoint +1);
			return s;
		default: throw new RuntimeException("STringTh: teenyness is broken");
		}
	}
	public String substring(int i, int j) {
		if (!SeventeenBitSafe) return this.value.substring(i,j);
		teenynessTest();
		switch(teenyness) {
		case SIXTEENY: 
			return this.value.substring(i,j);
		case TWENTYY:
			if (j-1 <= i) return "";
			int ithCodePoint = this.value.offsetByCodePoints(0, i);
			int jthCodePoint = this.value.offsetByCodePoints(ithCodePoint, j-1);
			String s = this.value.substring(ithCodePoint, jthCodePoint +1);
			return s;
		default: throw new RuntimeException("STringTh: teenyness is broken");
		}
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
		return obj != null && obj.getClass() == StringTh_17.class && ((StringTh_17) obj).value.equals(this.value);
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
	private static final int Cspan = 14;
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
		methodCode.put("span", Cspan);
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
	}

	@Override
	public Thing invokeMethod(String methodName, Thing[] args,   Syntax src)
			throws FisherException {
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
			case Cspan:
				return this.Mspan(methodName, args, evaller, frame, src);
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
			default:
				Doom.internal("Method key structure broken: " + methodC, src, methodC);
				return null;
			}
		}
		return this.tryThingMethods(methodName, args,  src);
	}

	private IntTh num() {
		return IntTh.of(this.length());
	}

	private StringTh_17 trim() {
		return StringTh_17.of(this.value.trim());
	}

	private Thing Cchar(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		// TODO - 17-bit-ify
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		int i = args[0].asJavaInt(src);
		int cp = this.value.codePointAt(i);
		return CharTh.of(cp, src);
	}
	private Thing Cjoin(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		String joined = Bard.sep(EvalUtil.iter(args[0], src), this.value);
		return StringTh_17.of(joined);
	}

	private DirTh Cdir(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return DirTh.of(value, src);
	}

	private StringTh_17 CplusChar(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		int i = args[0].asJavaInt(src);
		char c = (char) i;
		return StringTh_17.of(this.value + c);
	}
	private FileTh Cfile(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return FileTh.of(value);
	}

	private StringTh_17 CtoUpper(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return StringTh_17.of(value.toUpperCase());
	}

	private StringTh_17 CtoLower(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return StringTh_17.of(value.toLowerCase());
	}

	private StringTh_17 Ccapitalize(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
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
		return StringTh_17.of(c);
	}

	private StringTh_17 Cplus(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		String re = EvalUtil.toString(args[0]);
		return StringTh_17.of(value + re);
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
			RecordTh rg = RecordTh.make(src, matcherFields, IntTh.of(start), IntTh.of(end), StringTh_17.of(group));
			L.add(rg);
		}
		return ListTh.fromJavaList(L);
	}

	public static Thing thornifyMatcherForSlash(Syntax src, MatchResult m) throws FisherException {
		List<Thing> L = new ArrayList<Thing>();
		for (int g = 1; g <= m.groupCount(); g++) {
			String group = m.group(g);
			L.add(StringTh_17.of(group));
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

	private StringTh_17 Mreplace(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		String target = args[0].toString();
		String replacement = args[1].toString();
		return StringTh_17.of(this.value.replace(target, replacement));
	}

	private StringTh_17 Mreplace1(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		String target = args[0].toString();
		String replacement = args[1].toString();
		Doom.notYet("Sorry, didn't get aroudn to replace1.");
		return null;
	}

	private StringTh_17 MreplaceRE(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		String regexp = args[0].toString();
		String replacement = args[1].toString();
		return StringTh_17.of(this.value.replaceAll(regexp, replacement));
	}

	private StringTh_17 Mreplace1RE(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		String regexp = args[0].toString();
		String replacement = args[1].toString();
		return StringTh_17.of(this.value.replaceFirst(regexp, replacement));
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

	private StringTh_17 Mspan(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		int startMaybeNeg = (int) args[0].asLong(src);
		int start = pos(startMaybeNeg);
		int length = (int) args[1].asLong(src);
		int end = start + length;
		if (end > value.length())
			end = value.length();
		String ss = this.substring(start, end);
		return StringTh_17.of(ss);
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
		int i =  args[0].asJavaInt(src);
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
		if (args.length == 1) {
			long i = args[0].asLong(src);
			int pi = pos(i);
			if (pi >= len) {
				Doom.runtime("String index out of bounds: " + i + " on string of length " + len, src, this, i);
			}
			String sub = this.value.substring(pi, pi + 1);
			return StringTh_17.of(sub);
		} else if (args.length == 2) {
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
				return StringTh_17.EMPTY_STRING;
			} else if (pj < len) {
				sub = this.substring(pi, pj);
			} else {
				sub = this.substring(pi);
			}
			return StringTh_17.of(sub);
		} else {
			Doom.runtime("Wrong number of subscripts for string; it can take only 1 or 2.", src, this, args);
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
