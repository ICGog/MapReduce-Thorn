
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

import fisher.statics.ClassStatic;
import fisher.statics.Seal;
import fisher.statics.SealForClass;
import fisher.syn.ClassFormal;
import fisher.syn.ClsExtends;
import fisher.syn.Formals;
import fisher.syn.core.Id;

public interface Classlike extends Puretic {
	List<ClassFormal> params();
	Id name();
	public boolean hasParams();
	List<ClassMember> members();
	void setClassStatic(ClassStatic cs);
	List<ClsExtends> extendses();
	boolean isMarkedPure();
	
	ClassStatic classStatic(); 
	SealForClass classSeal();
	void setClassSeal(SealForClass classSeal);
	

}
