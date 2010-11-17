
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

import fisher.runtime.ClassDynamicTh;
import fisher.syn.core.Syntax;
import fisher.util.Doom;
import fisher.util.FisherException;
import fisher.util.SpecialCharacters;

public  class  SealForClass extends Seal  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	private ClassStatic classStatic;
	
	public SealForClass(Syntax def, String str, SealKind kind, Seal container, boolean builtIn) {
		super(def, str, kind, container, builtIn);
		assert (kind == SealKind.CLASS || kind == SealKind.ANON_OBJ_CLASS);
	} 
	
	@Override
	public String toString() {
		return 
			((kind == SealKind.CLASS) ? SpecialCharacters.CLASS : SpecialCharacters.ANON_OBJ)+ containerString() + this.str;
	}

	public ClassStatic classStatic() {
		return classStatic;
	}

	public void setClassStatic(ClassStatic classStatic) throws FisherException{
		if (this.classStatic != null) {
			Doom.internal("Duplicate setting of class static?", null);
		}
		this.classStatic = classStatic;
	}
	
	
}
