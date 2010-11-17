
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.runtime;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.syn.JavalyFun;
import fisher.syn.core.Syntax;
import fisher.util.Doom;
import fisher.util.FisherException;
import fisher.util.Security;

public  class  JavalyFunImpl extends ThingBuiltIn implements Applyable  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	public final Method method;
	public final JavalyFun decl;
	public final int arity;
	
	public JavalyFunImpl(Method method, JavalyFun decl) throws FisherException{
		super();
		Security.sandbag("No javaly funs allowed.");
		this.method = method;
		this.decl = decl;
		this.arity = decl.formals.size();
	}
	
	private JavalyFunImpl(Method m, JavalyFun d, int a) {
		super();
		this.method = m;
		this.decl = d;
		this.arity = a;		
	}
	
	public static JavalyFunImpl notDeclaredFromThorn(Method method, int arity)  {
		return new JavalyFunImpl(method, null, arity);
	}

	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		return false;
	}

	@Override
	public String typeString() {
		return "java function";
	}
	
	public String toString() {
		return decl.toString();
	}
	
	 public Thing apply(Thing[] args, Framelike frame, Evaller evaller, Syntax src) throws FisherException {
		if (args.length != arity) {
			Doom.runtime("Wrong number of arguments: wanted " + arity + "; got " + args.length, src, args);
		}
		Syntax oldSrc = ThingExtended.mySrc();
		try {
			ThingExtended.setSrc(src);
			Object res = method.invoke(null, args);
			return (Thing) res;
		} catch(InvocationTargetException e) {
			Doom.runtime("Invocation Target Exception about " + e.getTargetException(), src);
			return null;
		}
		catch (Exception e) {
			Doom.runtime("Error invoking java function: " + e, src, e);
			return null;
		}
		finally {
			ThingExtended.setSrc(oldSrc);
		}
	}

}
