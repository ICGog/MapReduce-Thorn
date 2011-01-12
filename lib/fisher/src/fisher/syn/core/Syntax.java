
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
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import fisher.ingest.Ingester;
import fisher.parser.*;
import fisher.syn.Cmd;
import fisher.syn.chart.SynPtr;
import fisher.syn.interfaces.ISyntax;
import fisher.syn.visitor.Visitor;
import fisher.syn.visitor.Walker;
import fisher.util.Bard;
import fisher.util.Compilation;
import fisher.util.CompilerMessage;
import fisher.util.DangerLevel;
import fisher.util.Doom;
import fisher.util.FisherException;
import fisher.util.FisherSource;

/**
 * @author bard
 * 
 */
public abstract  class  Syntax implements ISyntax  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	protected List<Syntax> children = new ArrayList<Syntax>();

	public final Token start;
	public final Token end;
	public FisherSource source;
	public boolean generated = false; // true for generated code, false for real code.  Used in tostring etc.
	public SynPtr geneology; // Explains how this is a child of its parent AST node.
	
	@Override
	public int hashCode() {
		return Math.abs(this.toString().hashCode());
	}

	public Syntax(Token start, Token end) {
		this.start = start;
		this.end = end;
	}
	
	public abstract String genericName();	
	
	
	public List<Syntax> children() {
		return  this.children; 
	}
	
	public boolean generated() {return this.generated;}

	public <ARG, RET, EXN extends Exception> RET accept(Visitor<ARG, RET, EXN> vis, ARG arg) throws EXN {
		return vis.visit((Syntax) this, arg);
	}

	public <ARG, EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
		vis.visit((Syntax) this, arg);
	}

	/**
	 * @return a very detailed string representation of this.
	 */
	public abstract String details();

	public static String detailsome(Object o) {
		if (o instanceof Syntax) {
			return ((Syntax) o).details();
		} else if (o == null) {
			return ("null");
		} else
			return o.toString();
	}

	protected String sep(Collection<?> C, String between) {
		return Bard.sep(C, between);
	}
	
	

	public static <T extends ISyntax> String sepDetails(Collection<T> c, String separator) {
		StringBuffer sb = new StringBuffer();

		for (Iterator<T> iter = c.iterator(); iter.hasNext();) {
			T element = iter.next();
			sb.append(element.details());
			if (iter.hasNext()) {
				sb.append(separator);
			}
		}
		return sb.toString();
	}
	
	public static String maybeNewLine() {
		return "\n";
	}
	

	/*
	 * The following functions are so that toString() will often return code
	 * right. (See the 'taut' tests in the ParserTest -- these are tests that
	 * parse(s).toString() equals s for carefully chosen s. In general it won't,
	 * it'll lose spacing and stuff, but it /can/. One tricky thing here is that
	 * sometimes expressions have semicolons after them (if they're statements),
	 * but there's no AST class distinguishing exp from stmtp. So,
	 * Syntax.doingStmt is a global flag telling whether I'm formatting
	 * something as EXP or STMT. So: stmt(C) returns C.toString() formatted as a
	 * statement, if the context calls for a asStmt(C) and asExp(C) sneak that
	 * info deep into recursive calls. Mostly you should use asStmt(C) and
	 * asExp(C).
	 */

	/**
	 * @param s
	 * @return s formatted as a statement
	 */
	public static String stmt(Cmd s) {
		if (s.explike() && doingStmt) {
			return s + ";" + maybeNewLine();
		} else
			return s.toString()+ maybeNewLine();
	}

	public static boolean doingStmt = false;

	public static String asStmt(ISyntax s) {
		boolean oldDoingStmt = doingStmt;
		doingStmt = true;
		String st = s instanceof Cmd ? stmt((Cmd) s) : s.toString();
		doingStmt = oldDoingStmt;
		return st;
	}
	
	public static String endingInSemi(String s) {
		int lastSemi = s.lastIndexOf(";");
		boolean endsSemiAlready = true;
		if (lastSemi < 0) {
			endsSemiAlready = false;
		}
		else {
			for (int i = lastSemi+1; i < s.length(); i++) {
				final char c = s.charAt(i);
				if (Character.isWhitespace(c)) continue;
				endsSemiAlready = false;
			}
		}
		if (endsSemiAlready) return s;
		else return s+";";
	}

	public static String asExp(ISyntax s) {
		boolean oldDoingStmt = doingStmt;
		doingStmt = false;
		String st = s instanceof Cmd ? stmt((Cmd) s) : s.toString();
		doingStmt = oldDoingStmt;
		return st;
	}

	public static <T extends ISyntax> String sepStmt(List<T> cmds, String separator) {
		StringBuffer sb = new StringBuffer();
		
		boolean first = true;
		for (Iterator<T> iter = cmds.iterator(); iter.hasNext();) {
			T element = iter.next();
			if (element.generated()) continue;
			if (!first) sb.append(separator);
			sb.append(asStmt(element)); //$NON-NLS-1$
			first  = false;
		}
		return sb.toString();
	}
	public static <T extends ISyntax> String sepExp(List<T> cmds, String separator) {
		StringBuffer sb = new StringBuffer();

		boolean first = true;
		for (Iterator<T> iter = cmds.iterator(); iter.hasNext();) {
			T element = iter.next();
			if (element.generated()) continue;
			if (!first) sb.append(separator);
			sb.append(asExp(element)); //$NON-NLS-1$
			first  = false;
		}
		return sb.toString();
	}

	public void setSourceOnFamily(FisherSource source) {
		this.source = source;
		for (Syntax syn : this.children) {
			syn.setSourceOnFamily(source);
		}
	}
	
	public String original() {
		return this.source.original(this);
	}

	public void flag(DangerLevel danger, String shortDesc, String longDesc, Object... details) {
		Compilation.flag1(danger, this, shortDesc, longDesc, details);
	}
	
	

	public void replace(Object newval) throws FisherException {
		if (this.geneology == null) {
			Doom.internal("Attempt to replace root object!", this, newval);
		}
		else {
			this.geneology.replaceWith(newval);
		}
	}
	
	public abstract void internalReplace(int field, int index, Object newval)throws FisherException ; 
	public abstract Object getChild(int field, int index)throws FisherException ; 
	public abstract List<SynPtr> synPtrs() ;
	
	public List<Syntax> computeDescendants() {
		List<Syntax> desc = new ArrayList<Syntax>();
		computeDescendantsInto(desc);
		return desc;
	}
	
	private void computeDescendantsInto(List<Syntax> desc) {
		desc.add(this);
		for (Syntax child : this.children()) {
			child.computeDescendantsInto(desc);
		}
	}
	
	public Syntax parent() {
		if (this.geneology == null) return null;
		return this.geneology.to; 
		}
	
	public void fillInParentPtrs()  throws FisherException {
		for (SynPtr childPtr : this.synPtrs()) {
			Object ochild = childPtr.fetch();
			if (ochild instanceof Syntax) {
				Syntax child = (Syntax) ochild;
				child.geneology = childPtr;
				child.fillInParentPtrs();
			}
		}
	}

	public abstract Syntax internalDeepCopy(Token start, Token end)  ;
	
	public final Syntax ingestedDeepCopy(Token start, Token end) {
		Syntax cp = this.internalDeepCopy(start, end);
		try {
			Ingester.check(cp);
		} catch (FisherException e) {
			e.printStackTrace();
		}
		return cp;
	}
	
	public static List deepCopyList(List<? extends ISyntax> L, Token start, Token end) {
		List O = new ArrayList(L.size());
		for (ISyntax syn : L) {
			Syntax ssyn = (Syntax) syn;
			Syntax dc = ssyn.ingestedDeepCopy(start, end);
			O.add(dc);
		}
		return O;
	}
	
	public void setGenerated() {
		this.generated = true;
	}
	
	public void allGenerated() {
		this.setGenerated();
		for(Syntax c : this.children()) c.allGenerated(); 
		
	}
	
	
}
