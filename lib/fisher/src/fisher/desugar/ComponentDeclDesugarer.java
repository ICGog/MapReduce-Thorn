
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.desugar;

import fisher.ingest.Ingester;
import fisher.parser.Token;
import java.util.*;
import fisher.syn.*;
import fisher.syn.core.Id;
import fisher.syn.interfaces.ProcMember;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  ComponentDeclDesugarer extends AbstractDesugarer  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public ComponentDeclDesugarer(Token start, Token end) {
		super(start, end);
		// TODO Auto-generated constructor stub
	}

	public static Cmd of(Token start, Token end, Id name, Formals formals, List<ProcMember> pms) {
		ComponentDeclDesugarer cdd = new ComponentDeclDesugarer(start, end);
		return cdd.of(name, formals, pms);
		// Original: component A {pms}
		// Desugared: A = {: ©©component©: fn () = spawn A {pms}, ©©hint©: "This is a desugared component decl." :}
	}
	
	public static final Id CCcomponentC = new Id(null,  DesugarUtils.GENSYM +DesugarUtils.GENSYM + "component" +DesugarUtils.GENSYM, true);
	public static final Id CChintC = new Id(null,  DesugarUtils.GENSYM +DesugarUtils.GENSYM + "hint" +DesugarUtils.GENSYM, true);
	
	public static Spawn retrieveSpawn(ComponentDecl cd) throws FisherException{
		try {
			Bind bind = (Bind) cd.actualCode;
			RecordCtor rec = (RecordCtor) bind.exp;
			RecordField rf1 = rec.fields.get(0);
			AnonFun fn = (AnonFun) rf1.exp;
			FunBody funBody = fn.fun;
			MonoBody monoBody = funBody.funbodies.get(0);
			Spawn spawn = (Spawn) monoBody.body;
			return spawn;
		} catch (ClassCastException e) {
			e.printStackTrace();
			Doom.runtime("Trying to get the anonymous function part of a component, but something's put together wrong.", cd);
			return null;
		}
	}
	public static Formals retrieveFormals(ComponentDecl cd) throws FisherException{
		try {
			Bind bind = (Bind) cd.actualCode;
			RecordCtor rec = (RecordCtor) bind.exp;
			RecordField rf1 = rec.fields.get(0);
			AnonFun fn = (AnonFun) rf1.exp;
			FunBody funBody = fn.fun;
			MonoBody monoBody = funBody.funbodies.get(0);
			return monoBody.formals;
		} catch (ClassCastException e) {
			e.printStackTrace();
			Doom.runtime("Trying to get the anonymous function part of a component, but something's put together wrong.", cd);
			return null;
		}
	}
	
	private Cmd of(Id name, Formals formals, List<ProcMember> pms) {
		// NOTE: 'retrieveSpawn' uses the following structure intimately.  Keep the two functions in sync.
		// This construction is done before ingesting, 'cause things aren't very well built yet.
		Spawn spawn = new Spawn(start, end, id(name), pms); // I think it's safe not to copy pms.
		
		//final Formals formals = new Formals(start, end, Collections.EMPTY_LIST);
		final MonoBody monoBody = new MonoBody(start, end, null, formals, null, null, false, 0, spawn, false, false);
		final FunBody funBody = new FunBody(start, end, Bard.list(monoBody), false);
		AnonFun fn = new AnonFun(start, end, funBody, false);
		Id compLbl = id(CCcomponentC);
		Id hintLbl = id(CChintC);
		RecordField rf1 = new RecordField(start, end, compLbl, fn);
		RecordField rf2 = new RecordField(start, end, hintLbl, lit("This is a desugared component declaration"));
		RecordCtor rec = new RecordCtor(start, end, Bard.list(rf1, rf2));
		Bind bind = new Bind(start, end, new PatVar(start, end, id(name)), rec);
		return bind;
	}
	
}
