
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

import java.util.List;

import fisher.eval.Frame;
import fisher.syn.Cmd;
import fisher.syn.FunDecl;
import fisher.syn.JavalyFun;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.syn.interfaces.ISyntax;
import fisher.syn.visitor.VanillaVisitCmd;
import fisher.syn.visitor.VanillaWalker;
import fisher.util.FisherException;

public  class  SealCircularDefs extends VanillaWalker<Env,  FisherException>  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public static void seal(List<? extends ISyntax> cmds, Env env) throws FisherException {
		SealCircularDefs sealcirc = new SealCircularDefs();
		for (ISyntax cmd : cmds) {
			((Syntax)cmd).accept(sealcirc, env);
		}
	}
	
	@Override
	public void visit(Syntax syn, Env arg) throws FisherException {
	}

	@Override
	public void visit(FunDecl syn, Env env) throws FisherException {
		Id id = syn.name;
		Seal idseal = SealMaker.of(syn, env.container);
		id.setSeal(idseal, syn);
		env.define(id.str(), idseal);
	}
	
	@Override
	public void visit(JavalyFun syn, Env env) throws FisherException {
		Id id = syn.name;
		Seal idseal = SealMaker.of(syn, env.container);
		id.setSeal(idseal, syn);
		env.define(id.str(), idseal);
		// And, while we're here, do the qualname that is the body too.
		for (Id qnid : syn.impl.ids) {
			qnid.setSeal(SealMaker.ofIdInDeadCode(qnid.str()), syn);
		}
		for (Id fmid : syn.formals) {
			fmid.setSeal(SealMaker.ofIdInDeadCode(fmid.str()), syn);
		}
	}

}
