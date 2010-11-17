
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.syn.chart;

import java.util.List;

import fisher.syn.core.Syntax;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  SynPtr  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public final fisher.syn.core.Syntax to;
	public final int field;
	public final int index;
	public SynPtr(Syntax to, int field, int index) {
		super();
		this.to = to;
		this.field = field;
		this.index = index;
	}
	
	public String toString() {
		return "SynPtr{field=" + field + " index= " + index + "to=" + to + "}" ; 
	}
	
	public Object fetch() throws FisherException {
		return to.getChild(field, index);
	}
	
	public void replaceWith(Object o) throws FisherException
	{
		if (o instanceof Syntax) {
			Syntax oldChild = (Syntax) this.fetch();
			Syntax newChild = (Syntax) o;
			if (newChild.parent() != null) {
				Doom.internal("No Syntax may have two parents!  Yet, somehow, it is desired that newChild does!", to,
						"newChild=", newChild, "oldChild=", oldChild, "field=" + field, "index=" + index);
			}
			newChild.geneology = this;
			List<Syntax> siblings = this.to.children();
			int i = siblings.indexOf(oldChild);
			siblings.set(i, newChild);
			}
		to.internalReplace(field, index, o);
	}
	
}
