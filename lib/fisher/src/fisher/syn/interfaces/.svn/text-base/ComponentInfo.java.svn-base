
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

import java.util.List;
import java.util.Set;

import fisher.statics.Seal;
import fisher.syn.FunDecl;
import fisher.syn.HighLevelCommunication;
import fisher.syn.ProcBody;
import fisher.syn.ProcInit;
import fisher.syn.core.Id;

public interface ComponentInfo {
	public List<ProcMember> bits();
    public List<LocalMember> localMembers() ;
    public List<HighLevelCommunication> highLevelCommunications() ;
    public List<FunDecl> funs();
    public ProcBody body() ;
    public ProcInit init() ;
    public Id name();
    public Set<Seal> freeVarSeals();
    public Id serveId();
    
}
