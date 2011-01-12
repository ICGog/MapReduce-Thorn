
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.eval.interfaces;

import fisher.runtime.Thing;
import fisher.syn.core.Syntax;
import fisher.util.FisherException;

public interface Fieldiferous {
	public Thing getField(String fieldName, Syntax src) throws FisherException;
	public boolean hasField(String fieldName);
}
