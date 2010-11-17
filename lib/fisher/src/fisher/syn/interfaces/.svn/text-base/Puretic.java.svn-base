
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.syn.interfaces;

import fisher.statics.purity.PurityStatus;
import fisher.syn.core.Id;
import fisher.syn.visitor.PureticWalker;

public interface Puretic extends ISyntax{
	public boolean isMarkedPure();
	public PurityStatus purityStatus();
	public void setPurityStatus(PurityStatus c);
	public Id idOfName(); // The Id of the name of this, if it has one, or null otherwise.
	
	public <ARG,  EXN extends Exception> void accept(PureticWalker<ARG, EXN> vis, ARG arg) throws EXN ;
}
