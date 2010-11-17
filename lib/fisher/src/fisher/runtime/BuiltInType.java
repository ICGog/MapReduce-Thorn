
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

import fisher.runtime.auxil.ImmutabilityContext;
import fisher.runtime.auxil.Typelike;
import fisher.statics.BuiltInTypeEnum;

public  class  BuiltInType extends ThingImmutable implements Typelike  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public final BuiltInTypeEnum type;
	
	
	public BuiltInType(BuiltInTypeEnum type) {
		super();
		this.type = type;
	}

	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		return true;
	}

	@Override
	public String typeString() {
		return type.name();
	}
	
	public boolean hasInstance(Thing thing) {
		return type.is(thing);
	}

}
