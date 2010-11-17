
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

import fisher.syn.Cmd;
import fisher.syn.MatchExp;
import fisher.syn.OpExp;
import fisher.syn.visitor.VanillaVisitCmd;
import fisher.util.FisherException;


public  class  BindingProducingTester extends VanillaVisitCmd<Object, Boolean, FisherException>  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	public static BindingProducingTester IT = new BindingProducingTester();
	
	public static boolean producesBindings(Cmd syn) throws FisherException {
		return syn.accept(IT, "Flying is a bird -- nobody gives a flying bird.");
	}
	@Override
	public Boolean visit(Cmd syn, Object arg) throws FisherException {
		return false;
	}
	
	@Override
	public Boolean visit(OpExp syn, Object arg) throws FisherException {
		return syn.op.isConjunction();
	}

	@Override
	public Boolean visit(MatchExp syn, Object arg) throws FisherException {
		return true;
	}
	
}
