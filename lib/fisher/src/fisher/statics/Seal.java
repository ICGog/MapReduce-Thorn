
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.statics;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import fisher.eval.interfaces.Framelike;
import fisher.runtime.Frameable;
import fisher.runtime.Thing;
import fisher.syn.ImportStmt;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.util.Bard;
import fisher.util.FisherException;
import fisher.util.SpecialCharacters;

public  class  Seal  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	// An Id's Seal is what the compiler knows about that Id.
	// Two Ids with the same Seal (in the same dynamic frame) have the same value.
	
	
	/**
	 * The container is the seal of the static thing which encloses this identifier.
	 * For example, in module M { var x; }, 
	 * x's seal's container = M's seal.
	 * The container may be null, for uncontained things.
	 */
	public final Seal container;
	
	/**
	 * If contents != null, contents is a list of the seals whose container is this.
	 */
	public  List<Seal> contents = null;
	
	/**
	 * A Seal's def should be a human-grokkable bit of syntax which defines it.
	 */
	public final Syntax def; 

	/**
	 * This is its print name: 
	 */
	protected final String str;
	
	public final boolean builtIn;
	
	private ImportStmt importStmt;
	

	public String str() { return this.str; }
	
	public final SealKind kind;

	public Seal(Syntax def, String str, SealKind kind, Seal container, boolean builtIn) {
		super();
		this.def = def;
		this.str = str;
		this.kind = kind;
		this.container = container;
		this.builtIn = builtIn;
		if (container != null) container.addContents(this);
		SealUtils.register(this);
		
	}
	
	public Seal pwnClone(Seal pwner) {
		return new Seal(this.def, this.str, this.kind, pwner, this.builtIn);
	}
	
	private void addContents(Seal newCont) {
		if (this.contents == null) {
			this.contents = Bard.list(newCont);
		}
		else {
			this.contents.add(newCont);
		}
	}
	
	public Seal sealOfContained(String name) {
		if (contents == null) return null;
		for (Seal cont : contents) {
			if (cont.str().equals(name)) return cont;
		}
		return null;
	}
	
	public String toString() {
		return SpecialCharacters.SEAL +
			containerString() + 
			str 
			// + "“" + kind + "”" + "{" + (def == null ? "" : def.parent()) + "}"
			;
	}
	
	protected String containerString() {
		return this.container == null ? "" : this.container.containerString() + this.container.str + ".";
	}
	
	public final boolean equals(Object o) {
		return this == o;
	}
	

	
	public SealAlias alias(String newName) {
		return new SealAlias(this.def, newName, this.kind, 
				this.container, 
				this.str, this);
	}

	public Seal dealias() {
		return this;
	}
	
	public boolean isAnAliasFor(Seal other) {
		return this == other;
	}
	
	public void check(Framelike framelike, Frameable f, Syntax src) throws FisherException {
		// Nothing wrong here!
	}
	
	public List<Thing> types(Framelike framelike, Syntax src)  throws FisherException{
		return Collections.EMPTY_LIST;
	}

	public ImportStmt getImportStmt() {
		return importStmt;
	}

	public void setImportStmt(ImportStmt importStmt) {
		this.importStmt = importStmt;
	}
}
