
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

import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.BoolTh;
import fisher.runtime.BuiltInFunctionTh;
import fisher.runtime.ObjectTh;
import fisher.runtime.RecordTh;
import fisher.runtime.Thing;
import fisher.syn.core.Syntax;
import fisher.util.FisherException;

public  class  Same_type_and_All_fields_equal_BIF extends BuiltInFunctionTh  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	@Override
	public Thing apply(Thing[] args, Framelike ignoredFrame, Evaller evaller, Syntax src) throws FisherException {
		checkNumberOfArgs(2, 2, "same_type_and_all_fields_equal?", args, evaller, ignoredFrame, src);
		Thing a = args[0];
		Thing b = args[1];
		if(a.isRecord() && b.isRecord()) return compareRecords(a.asRecord(src),b.asRecord(src));
		if(a instanceof ObjectTh && b instanceof ObjectTh) {
			return compareObjects( (ObjectTh)a, (ObjectTh) b);
		}
		return BoolTh.False;
	}

	private static BoolTh compareRecords (RecordTh a, RecordTh b) {
		return BoolTh.of(a.equals(b));
	}
	
	private static BoolTh compareObjects(ObjectTh a, ObjectTh b) throws FisherException {
		return BoolTh.of(a.sameTypeAndFieldsAs(b));
	}
	
	
}
