
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.runtime.lib;

import java.io.Serializable;

import org.w3c.dom.Document;
import org.w3c.dom.Node;

import fisher.runtime.ThingExtended;
import fisher.runtime.auxil.ImmutabilityContext;

public abstract  class  XMLAbstract extends ThingExtended  implements Serializable { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	final public Document domulate() {
		Document doc = XMLFuns.docBldr().newDocument();
		this.domulateUnder(doc, doc);
		return doc;
	}
	
	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		return true;
	}
	
	abstract /*package*/ void domulateUnder(Document doc, Node node);
}
