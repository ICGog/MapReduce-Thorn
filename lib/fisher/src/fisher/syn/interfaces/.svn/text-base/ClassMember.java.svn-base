
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

import fisher.syn.ClsCtorDef;
import fisher.syn.visitor.ClassMemberVisitor;
import fisher.syn.visitor.ClassMemberWalker;

public interface ClassMember extends ISyntax{
	public <ARG, RET, EXN extends Exception> RET accept(ClassMemberVisitor<ARG, RET, EXN> vis, ARG arg) throws EXN ;

	public <ARG, EXN extends Exception> void accept(ClassMemberWalker<ARG, EXN> vis, ARG arg) throws EXN ;
}
