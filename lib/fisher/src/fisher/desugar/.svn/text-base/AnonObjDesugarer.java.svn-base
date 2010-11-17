
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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import fisher.parser.Token;
import fisher.syn.*;
import fisher.syn.converters.AddImplicitClassMembers;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.syn.interfaces.ClassMember;
import fisher.syn.interfaces.ObjectMember;
import fisher.util.Bard;

public  class  AnonObjDesugarer extends AbstractDesugarer  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	private AnonObjDesugarer(Token start, Token end) {
		super(start, end);
	}

	public static ClsDecl retrieveGeneratedClsDecl(AnonObj anon){
		// This function rips the generated code apart to get at the class declaration.
		Valof valof = (Valof) anon.actualCode;
		ClsDecl clsdecl = (ClsDecl) valof.stmts.get(0);
		return clsdecl;
	}
	
	
	public static Cmd of(Token start, Token end, List<ClsExtends> exts, List<ObjectMember> members, boolean isVal){
		// object extends A(a) { val b = c; method m()=d; }
		// -->
		//   valofInner{
		//     class ©obj© extends A(a) { val b = c; method m()=d; new ©obj©() = {new@A(a);}}
		//     ©obj©();
		//   }
		// If you change this, change retrieveGeneratedClsDecl too.
		AnonObjDesugarer de = new AnonObjDesugarer(start, end);
		AddImplicitClassMembers.to((List<ClassMember>)(List)members, start, end, null,false,  null, exts, isVal);
		return de.of(exts, members, isVal);
	}
	
	private Cmd of (List<ClsExtends> exts, List<ObjectMember> members, boolean isPure){
		Id objClassName = DesugarUtils.gensym(start, "object");
		
		List<ClassMember> clsMembers = new ArrayList<ClassMember>();
		
		for (ObjectMember om : members) {
			ClassMember cm = (ClassMember)( ((Syntax)om).ingestedDeepCopy(start, end));
			clsMembers.add( cm);
		}
		
		List<ClsExtends> clsExt = new ArrayList<ClsExtends>();
		
		List<Cmd> newSuperCallsForCtor = new ArrayList<Cmd>();
		
		for (ClsExtends e : exts) {
			clsExt.add(new ClsExtends(start, end, qn(e.superName), Collections.EMPTY_LIST));
			SuperCtorCall newCall = new SuperCtorCall(start, end, qn(e.superName), deepCopyCmds(e.args));
			newSuperCallsForCtor.add(newCall);
		}
	
		Seq ctorBody = new Seq(start, end, newSuperCallsForCtor);		
		
		Formals noFormals = new Formals(start, end, Collections.EMPTY_LIST);
		
		MonoBody ctorMonobody = new MonoBody(start, end, id(objClassName), noFormals, 
				null, null, false, 0, // Distribution stuff -- not here.
				ctorBody, false, isPure); 
		ClsCtorDef newDef = new ClsCtorDef(start, end, id(objClassName), ctorMonobody);
		clsMembers.add(newDef);
		
		ClsDecl clsDecl = new ClsDecl(start, end, isPure,  id(objClassName), false, Collections.EMPTY_LIST, clsExt,clsMembers);
		
		FunCall ctorCall = new FunCall(start, end, varexp(objClassName), Collections.EMPTY_LIST);
		
		Valof seq = new Valof(start, end, Bard.list(clsDecl, ctorCall), true);
		
//		System.out.println("AnonObjDesugarer ... \n"  + seq);
		
		return seq;
	}
}
