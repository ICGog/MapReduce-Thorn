
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

import java.util.List;

import fisher.eval.EvalUtil;
import fisher.statics.Seal;
import fisher.syn.core.Syntax;
import fisher.util.Doom;
import fisher.util.SpecialCharacters;

public  class  VarCell extends Frameable  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	private Thing cargo;
	public Seal seal;
	private final List<Thing> types;

	public VarCell(Thing cargo, Seal seal, List<Thing> types) {
		super();
		this.cargo = cargo;
		this.seal = seal;
		this.types = types;
	}

	public Thing cargo() {
		return cargo;
	}

	public void setCargo(Thing newCargo, Syntax src) throws fisher.util.FisherException {
		EvalUtil.confirmTypes(newCargo, types, src);
		this.cargo = newCargo;
	}

	public String toString() {
		return SpecialCharacters.VAR_CELL + this.seal.str() + "[" + cargo + "]";
	}

	@Override
	public Thing Rvalue() {
		return cargo;
	}
}
