
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

import fisher.eval.EvalUtil;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.Frameable;
import fisher.runtime.Thing;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  SealTyped extends Seal  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public final List<fisher.syn.core.Id> types;

	public SealTyped(Syntax def, String str, SealKind kind, Seal container, boolean builtIn, List<Id> types) {
		super(def, str, kind, container, builtIn);
		this.types = types;
	}
	
	public Seal pwnClone(Seal pwner) {
		return new SealTyped(this.def, this.str, this.kind, pwner, this.builtIn, this.types);
	}
	
	
	public String toString() {
		return super.toString() + 
			(types.isEmpty() ? "" : ":"+Bard.sep(types, "&"));
	}
	
	public void check(Framelike framelike, Frameable f, Syntax src) throws FisherException {
		Thing fthing = f == null ? null : f.Rvalue(); 
		for (Id type : types) {
			Seal typeseal = type.seal();
			Frameable frtype = framelike.baseValue(typeseal, src);
			if (fthing != null && ! EvalUtil.thingIsType(fthing, frtype, src)) {
				Doom.runtime("Wrong type: " 
						+ this.str + " is declared to always be " + type 
						+ " but here I am trying to make it have value " + fthing 
						+ " which is a " + fthing.typeString() + " and not a " + type + " at all."
						, src, fthing);
			}
		}
	}
	public List<Thing> types(Framelike framelike, Syntax src) throws FisherException {
		List<Id> types = this.types;
		return EvalUtil.computeTypes(framelike, src, types);
	}

	

}
