
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

import java.util.Collections;

import fisher.parser.Token;
import fisher.syn.*;
import fisher.syn.core.ColAccess;
import fisher.syn.core.ColSpecial;
import fisher.syn.core.Id;
import fisher.util.Bard;

public  class  DesugarMap extends AbstractDesugarer  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	private DesugarMap(Token start, Token end) {
		super(start, end);
		// TODO Auto-generated constructor stub
	}
	
	public static Cmd of(Token start, Token end) {
		DesugarMap dm = new DesugarMap(start, end);
		return dm.dolt();
	}
	
	private Cmd dolt() {
		// table(k){map var v;}
		Id v = new Id(start,  "v");
		IdWithOptInit v2 = new IdWithOptInit(start, end, v, null, tycon(v), ColAccess.VAR);
		TableFields mapvarv = new TableFields(start, end, ColAccess.VAR, ColSpecial.MAP, Bard.list(v2));
		Id k = new Id(start, "k");
		TableKey tk = new TableKey(start, end, k, new TypeConstraints(start, end, Collections.EMPTY_LIST));
		Table map = new Table(start, end, Bard.list(tk), Bard.list(mapvarv));
		return map;
	}
	
}
