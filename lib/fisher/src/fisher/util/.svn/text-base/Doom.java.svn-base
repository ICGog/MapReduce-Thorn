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

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.parser.Token;
import fisher.runtime.ObjectTh;
import fisher.runtime.StringTh;
import fisher.runtime.Thing;
import fisher.runtime.auxil.ThornThrow;
import fisher.syn.core.Syntax;
import fisher.test.TestUtils;

public class Doom {
	static String copyright() {
		return fisher.util.Copyright.IBM_COPYRIGHT;
	}

	public static String locator(Token tok) {
		return "line " + tok.beginLine + " column " + tok.beginColumn;
	}

	public static Thing notYet() throws RuntimeException {
		throw new RuntimeException("Not yet implemented!");
	}

	public static Thing notYet(String s) throws RuntimeException {
		throw new RuntimeException("Not yet implemented!\n" + s + "\n");
	}

	public static FisherException internal(String msg, fisher.syn.interfaces.ISyntax isrc, Object... notes)
			throws FisherInternalCompilerDoom {
		if (isrc == null)
			isrc = Evaller.lastSyntax();
		Syntax src = (Syntax) isrc;
		String s = msg + "\nSource = " + src + "\nfrom = " + locator(src.start)
				+ (src.start != src.end ? "\nto = " + locator(src.end) : "")
				+ (notes.length == 0 ? "" : "\n" + Bard.sep(notes, "\n"));
		throw new FisherInternalCompilerDoom(s);
	}

	public static void internalCatastrophe(String msg, Syntax src, Object... notes) throws RuntimeException {
		if (src == null) {
			throw new RuntimeException(msg + (notes.length == 0 ? "" : "\n" + Bard.sep(notes, "\n")));
		}
		String s = msg + "\nSource = " + src + "\nfrom = " + locator(src.start)
				+ (src.start != src.end ? "\nto = " + locator(src.end) : "")
				+ (notes.length == 0 ? "" : "\n" + Bard.sep(notes, "\n"));
		throw new RuntimeException(s);
	}

	public static FisherException throwy(Object o, Syntax src) throws ThornThrow {
		try {
			throw new ThornThrow(EvalUtil.thingify(o, src), src);
		} catch (Exception e) {
			throw new ThornThrow(StringTh.of(o.toString()), src);
		}
	}

	public static Thing runtime(String msg, Syntax src, Object... notes) throws FisherException {
		if (src == null)
			src = Evaller.lastSyntax();
		if (src == null) {
			String s = msg +"\nUnknown Source\n"+ (notes.length == 0 ? "" : "\n" + Bard.sep(notes, "\n"));
			throw new ThornThrow(StringTh.of(s), null);
		} else {
			String s = msg + "\nSource = " + src + "\nfrom = " + locator(src.start)
					+ (src.start != src.end ? "\nto = " + locator(src.end) : "")
					+ (notes.length == 0 ? "" : "\n" + Bard.sep(notes, "\n"));
			throw new ThornThrow(StringTh.of(s), src);
		}
	}

	public static Thing runtimeNonThorn(String msg, Syntax src, Object... notes) throws RuntimeException {
		if (src == null)
			src = Evaller.lastSyntax();
		String s = msg + "\nSource = " + src + "\nfrom = " + (src == null ? " dunno " : locator(src.start))
				+ (src == null ? "" : (src.start != src.end ? "\nto = " + locator(src.end) : ""))
				+ (notes.length == 0 ? "" : "\n" + Bard.sep(notes, "\n"));
		throw new RuntimeException(s);
	}

	public static String longString(Syntax src) {
		if (src == null)
			src = Evaller.lastSyntax();
		return "\nSource = " + src + "\nfrom = " + locator(src.start)
				+ (src.start != src.end ? "\nto = " + locator(src.end) : "");
	}

	public static void testFailure(String msg, Syntax src, Object... notes) throws FisherTestFailure {
		if (src == null)
			src = Evaller.lastSyntax();
		String s = "Test Failure at " + TestUtils.loc + "\n" + msg + longString(src)
				+ (notes.length == 0 ? "" : "\n" + Bard.sep(notes, "\n"));
		throw new FisherTestFailure(s);
	}

	public static ObjectTh noThis(Evaller evaller, Syntax src) throws FisherRuntimeException {
		if (src == null)
			src = Evaller.lastSyntax();
		throw new FisherRuntimeException("No 'this' is available here: " + longString(src));
	}

	public static ObjectTh nullPointer(Evaller evaller, Syntax src) throws FisherRuntimeException {
		if (src == null)
			src = Evaller.lastSyntax();
		throw new FisherRuntimeException("Null pointer in " + longString(src));
	}

	public static Syntax maybeAt;

	public static Syntax oldAt(Syntax newAt) {
		Syntax o = maybeAt;
		maybeAt = newAt;
		return o;
	}

	public static void at(Syntax src) {
		maybeAt = src;
	}

	public static void guessedSource(Exception exn, String text, Object... notes) {
		throw new RuntimeException(text + "\n" + "Which might have been caused somewhere around " + longString(maybeAt)
				+ "\nNotes: \n" + Bard.sep(notes, "\n") + exn.toString());
	}

}
