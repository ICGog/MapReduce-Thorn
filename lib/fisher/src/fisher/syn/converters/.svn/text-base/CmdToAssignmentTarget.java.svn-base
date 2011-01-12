
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.syn.converters;

import fisher.syn.AssignTarget;
import fisher.syn.AssignToFieldOfSubscripted;
import fisher.syn.AssignToId;
import fisher.syn.AssignToMap;
import fisher.syn.AssignToSubscripted;
import fisher.syn.AssignTofield;
import fisher.syn.BracketCall;
import fisher.syn.Cmd;
import fisher.syn.FieldRef;
import fisher.syn.FunCall;
import fisher.syn.VarExp;
import fisher.syn.visitor.VanillaVisitCmd;

public  class  CmdToAssignmentTarget extends VanillaVisitCmd<Object, AssignTarget, RuntimeException>  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	@Override
	public AssignTarget visit(Cmd syn, Object arg) throws RuntimeException {
		return null;
	}

	@Override
	public AssignTarget visit(VarExp syn, Object arg) throws RuntimeException {
		return new AssignToId(syn.start, syn.end, syn.id);
	}
	
	@Override
	public AssignTarget visit(FieldRef syn, Object arg) throws RuntimeException {
		if (syn.target instanceof FunCall) {
			FunCall fc = (FunCall) syn.target;
			return new AssignToFieldOfSubscripted(syn.start, syn.end, fc.function, fc.args, syn.field );
			
		}
		return new AssignTofield(syn.start, syn.end, syn.target, syn.field);
	}
	
	@Override
	public AssignTarget visit(FunCall syn, Object arg) throws RuntimeException {
		return new AssignToSubscripted(syn.start, syn.end, syn.function, syn.args);
	}
	
	@Override
	public AssignTarget visit(BracketCall syn, Object arg) throws RuntimeException {
		return new AssignToMap(syn.start, syn.end, syn.function, syn.args);
	}
	
}
