
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.eval;

import com.sun.org.omg.SendingContext.CodeBasePackage.ValueDescSeqHelper;

import fisher.runtime.ClassDynamicTh;
import fisher.syn.Bind;
import fisher.syn.ClsCtorDef;
import fisher.syn.ClsDecl;
import fisher.syn.ClsPatDef;
import fisher.syn.ImportStmt;
import fisher.syn.MethDecl;
import fisher.syn.VarDecl;
import fisher.syn.core.Syntax;
import fisher.syn.interfaces.ClassMember;
import fisher.syn.interfaces.Classlike;
import fisher.syn.visitor.ClassMemberWalker;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  ClassDynamizer implements ClassMemberWalker<ClassDynamicTh, FisherException>  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	public static ClassDynamicTh instantiate(Classlike cd, fisher.eval.interfaces.Framelike frame) throws FisherException {
		ClassDynamicTh classDynamic = new ClassDynamicTh(cd, frame);
		ClassDynamizer dynamizer = new ClassDynamizer(); // TODO -- can I reuse the dynamizer?
		for (ClassMember cm : cd.members()) {
			cm.accept(dynamizer, classDynamic);
		}
		return classDynamic;
	}

	/* (non-Javadoc)
	 * @see fisher.syn.visitor.ClassMemberWalker#visit(fisher.syn.ClsCtorDef, java.lang.Object)
	 */
	public void visit(ClsCtorDef syn, ClassDynamicTh arg) throws FisherException {
		// Nothing to do for constructors, here.
	}

	/* (non-Javadoc)
	 * @see fisher.syn.visitor.ClassMemberWalker#visit(fisher.syn.ClsPatDef, java.lang.Object)
	 */
	public void visit(ClsPatDef syn, ClassDynamicTh arg) throws FisherException {
		// Nothing to do for pats, here.		
	}

	/* (non-Javadoc)
	 * @see fisher.syn.visitor.ClassMemberWalker#visit(fisher.syn.ClsValFieldDecl, java.lang.Object)
	 */
	public void visit(Bind syn, ClassDynamicTh arg) throws FisherException {
		// Nothing to do.
	}

	/* (non-Javadoc)
	 * @see fisher.syn.visitor.ClassMemberWalker#visit(fisher.syn.ClsVarFieldDecl, java.lang.Object)
	 */
	public void visit(VarDecl syn, ClassDynamicTh classDynamic) throws FisherException {
		// NOT: Computer.doVarDecl(syn, syn.val, syn.id, classDynamic, classDynamic.frame().evaller);
		// We'll do that when we create an instance of the class.
	}

	/* (non-Javadoc)
	 * @see fisher.syn.visitor.ClassMemberWalker#visit(fisher.syn.ImportStmt, java.lang.Object)
	 */
	public void visit(ImportStmt syn, ClassDynamicTh arg) throws FisherException {
		// TODO Auto-generated method stub
		// Here, in fact, I think there is something to do.
		Doom.notYet();	
	}

	/* (non-Javadoc)
	 * @see fisher.syn.visitor.ClassMemberWalker#visit(fisher.syn.MethDecl, java.lang.Object)
	 */
	public void visit(MethDecl syn, ClassDynamicTh arg) throws FisherException {
		// Nothing to do for methods
	}
	
	

}
