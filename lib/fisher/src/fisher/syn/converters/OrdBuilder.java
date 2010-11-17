
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.syn.converters;

import java.util.Collections;

import fisher.parser.Token;
import fisher.syn.IdWithOptInit;
import fisher.syn.Ord;
import fisher.syn.TableFields;
import fisher.syn.TypeConstraint;
import fisher.syn.TypeConstraints;
import fisher.syn.core.ColAccess;
import fisher.syn.core.ColSpecial;
import fisher.syn.core.Id;
import fisher.util.Bard;

public  class  OrdBuilder  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public static Ord simple(Token start, Token end) {
		
		IdWithOptInit idwi = new IdWithOptInit(start, end, new Id(start, "v"), null,
				new TypeConstraints(start, end, Collections.EMPTY_LIST),
				ColAccess.VAR);
		TableFields flds = new TableFields(start, end, ColAccess.VAR, ColSpecial.MAP, Bard.list(idwi) );
		Ord ord = new Ord(start, end, Bard.list(flds));
		return ord;
	}
}
