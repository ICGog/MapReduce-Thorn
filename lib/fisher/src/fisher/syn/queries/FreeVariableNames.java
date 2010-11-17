
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.syn.queries;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import fisher.syn.ClsExtends;
import fisher.syn.Cmd;
import fisher.syn.Literal;
import fisher.syn.VarExp;
import fisher.syn.core.Syntax;
import fisher.syn.interfaces.ISyntax;
import fisher.syn.visitor.VanillaVisitCmd;
import fisher.syn.visitor.VanillaVisitor;
import fisher.syn.visitor.VanillaWalker;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  FreeVariableNames extends VanillaWalker<Set<String>, RuntimeException>  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public static Set<String> of(List<? extends ISyntax> syns) {
		Set<String> S = new HashSet<String>();
		for (ISyntax s : syns) {
			S.addAll(of((fisher.syn.core.Syntax)s));
		}
		return S;
	}
	
	private static FreeVariableNames IT = new FreeVariableNames();
	
	public static Set<String> of(Syntax is)  {
		Set<String> S = new HashSet<String>();
		is.accept(IT, S);
		return S;
	}
	
	@Override
	public void visit(Syntax syn, Set<String> arg) throws RuntimeException {
		Doom.notYet();
	}
	
	public Set<String> fromKids(Syntax syn,Set<String> S) {
		for (Syntax kid  : syn.children()) {
			kid.accept(this, S);
		}
		return S;
	}
	
	public Set<String> from(Collection<? extends Syntax> syns,Set<String> S) {
		for (Syntax kid  : syns) {
			kid.accept(this, S);
		}
		return S;
	}
	
	@Override
	public void visit(ClsExtends syn, Set<String> arg) throws RuntimeException {
		from(syn.args, arg);
	}
	
	@Override
	public void visit(VarExp syn, Set<String> arg) throws RuntimeException {
		arg.add(syn.id.str());
	}
	
	@Override
	public void visit(Literal syn, Set<String> arg) throws RuntimeException {
	}
}

