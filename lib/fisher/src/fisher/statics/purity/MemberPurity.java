
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.statics.purity;

import fisher.syn.AnonObj;
import fisher.syn.Bind;
import fisher.syn.ClsCtorDef;
import fisher.syn.ClsDecl;
import fisher.syn.ClsPatDef;
import fisher.syn.ImportStmt;
import fisher.syn.MethDecl;
import fisher.syn.VarDecl;
import fisher.syn.visitor.ClassMemberVisitor;
import fisher.syn.visitor.ClasslikeVisitor;
import fisher.util.FisherException;
import static fisher.statics.purity.PurityStatus.*;
import static fisher.syn.interfaces.Puretic.*;


public  class  MemberPurity extends Object implements ClassMemberVisitor<MemberPurity.Zonk, PurityStatus, FisherException>  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	public static class Zonk {}
	
	public static Zonk zonk = new Zonk();
	
	
	public static final MemberPurity IT = new MemberPurity();

	public PurityStatus visit(Bind syn, Zonk arg) throws FisherException {
		return INNOCENT;
	}
	
	public PurityStatus visit(ClsCtorDef syn, Zonk arg) throws FisherException {
		return INNOCENT;
	}
	
	public PurityStatus visit(ClsPatDef syn, Zonk arg) throws FisherException {
		return INNOCENT;
	}
	
	public PurityStatus visit(ImportStmt syn, Zonk arg) throws FisherException {
		return INNOCENT;
	}
	
	public PurityStatus visit(MethDecl syn, Zonk arg) throws FisherException {
		return INNOCENT;
	}
	
	public PurityStatus visit(VarDecl syn, Zonk arg) throws FisherException {
		return IMPURE;
	}
}
