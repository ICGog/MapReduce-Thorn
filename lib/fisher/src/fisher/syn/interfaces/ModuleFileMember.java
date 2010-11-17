
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

import fisher.syn.visitor.ModuleFileMemberVisitor;
import fisher.syn.visitor.ModuleFileMemberWalker;

public interface ModuleFileMember extends ISyntax {
	public <ARG,  EXN extends Exception>void  accept(ModuleFileMemberWalker<ARG, EXN> walker, ARG arg ) throws EXN;
}
