
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.runtime.auxil;

import fisher.eval.interfaces.Fieldiferous;
import fisher.runtime.RecordTh;
import fisher.runtime.TableTh;
import fisher.runtime.Thing;
import fisher.syn.core.Syntax;
import fisher.util.Bard;
import fisher.util.FisherException;

public  class  Row  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public TableTh table;
	public ColInfo colInfos[];
	public Thing[] values;
	
	
	
	private Row(TableTh table, ColInfo[] colInfos, Thing[] values) {
		super();
		this.table = table;
		this.colInfos = colInfos;
		this.values = values;
	}



	public static Row of(TableTh table, Fieldiferous ff, Syntax src) throws FisherException {
		ColInfo[] colInfos = table.colInfos;
		Thing[] values = new Thing[colInfos.length];
		for (int i = 0; i < colInfos.length; i++) {
			ColInfo ci = colInfos[i];
			values[i] = ff.getField(ci.name, src);
		}
		return new Row(table, colInfos, values);
	}
	
	public String toString() {
		return "[" + Bard.sep(values, ",") + "]";
	}
	
	public RecordTh asRecordTh()  {
		return RecordTh.makeFromRow(colInfos, values);
	}
	
	
	public RecordTh keyRecord() {
		return RecordTh.makeFromSomeColumns(table.keyInfos, values);
	}
	
	public void mutateField(ColInfo fieldToMutate, Thing newValue) {
		values[fieldToMutate.pos] = newValue;
	}
	
	public boolean equals(Object other) {
		if (other instanceof Row) {
			Row oro = (Row) other;
			final RecordTh thirec = this.asRecordTh();
			final RecordTh ororec = oro.asRecordTh();
			return thirec.equals(ororec);			
		}
		return false;
	}
	
}
