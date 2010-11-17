
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
import java.util.Collections;
import java.util.List;

import fisher.desugar.DesugarUtils;
import fisher.parser.*;
import fisher.statics.Seal;
import fisher.syn.chart.SynPtr;
import fisher.util.DangerLevel;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  Id extends Syntax  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	public Id(Token id) {
		this(id, id.image);
	}
	
	public Id(Token id, String str) {
		this(id, str, false);
	}
	
	public Id(Token id, String str, boolean isGensym) {
		super(id,id);
		this.str = str;
		this.gensymmed = isGensym;
		boolean hasGensymChar = str.indexOf(DesugarUtils.GENSYM) >= 0;
		if (isGensym != hasGensymChar) {
			Doom.internalCatastrophe("Misuse of gensym character " + DesugarUtils.GENSYM, this);
		}
	}
	
	public static boolean isAcceptableIdName(String str) {
		int indexOf = str.indexOf(DesugarUtils.GENSYM);
		return indexOf == -1;
	}
	
	public static String messageAbout(String str) {
		if (!isAcceptableIdName(str)) {
			return "No identifier can use the character '" + DesugarUtils.GENSYM+"'"; 
		}
		else return null;
	}
	
	private final String str;
	
	public final boolean gensymmed;
	
	private Seal seal = null;
	
	public final String str() {
		return this.str;
	}
	
	public Seal seal() {return this.seal;}
	
	public void setSeal(Seal seal, Syntax src) throws FisherException {
		if (this.seal != null && this.seal != seal) {
			src.flag(DangerLevel.ERROR, "duplicate definitions for " + seal.str(), 
					"One definition is " + seal.def + "\n"
					+ "The other is " + src,
					src);
		}
		this.seal = seal;
	}
	
	@Override
	public String details() {
		// TODO Auto-generated method stub
		return this.str();
	}
	
	private String cachedToString = null;
	
	public String toString() {
		if (cachedToString != null) return cachedToString;
		if(str.matches("^[a-zA-Z_][a-zA-Z_0-9]*$")) 
			cachedToString = str;
		else cachedToString = "`" + str + "`";
		return cachedToString;
	}
	
	@Override
	public boolean equals(Object obj) {
		return (obj != null && (obj.getClass()) == Id.class) && ((Id)obj).str().equals(this.str());
	}
	
	@Override
	public int hashCode() {
		return str.hashCode();
	}
	
	@Override
	public Object getChild(int field, int index) throws FisherException {
		Doom.internal("Ids don't have children", this, field, index);
		return null; 
	}
	
	@Override
	public void internalReplace(int field, int index, Object newval) throws FisherException {
		Doom.internal("Ids can't get replaced", this, field, index, newval);
	}
	
	@Override
	public List<SynPtr> synPtrs() {
		return Collections.EMPTY_LIST;
	}
	

	public Id internalDeepCopy(Token start, Token end) {return new Id(start, this.str(), this.gensymmed); }
	
	@Override
	public String genericName() {
		return "Id";
	}
	
}
