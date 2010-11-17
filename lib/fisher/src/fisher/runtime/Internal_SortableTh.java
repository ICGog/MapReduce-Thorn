
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

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  Internal_SortableTh extends ThingBuiltIn implements Comparable<Internal_SortableTh>  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public Thing payload;
	public Thing[] keys;
	public BoolTh[] ascending;
	public Syntax src; // For debugging, and only sane 'cause it's an internal class.

	@Override
	public String typeString() {
		return "internal_sortable";
	}	
	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		return true;
	}

	public int compareTo(Internal_SortableTh that) {
		try {
			if (this.keys.length != that.keys.length || this.ascending.length != this.keys.length
					|| that.ascending.length != that.keys.length) {
				Doom.internalCatastrophe("Comparison of Internal_SortableTh's of different size?", src, this, that);
			}
			for (int i = 0; i < this.keys.length; i++) {
				Thing thisKey = this.keys[i];
				Thing thatKey = that.keys[i];
				boolean ascending = this.ascending[i].asBoolean(src);
				int thisToThat = (int) EvalUtil.compare(thisKey, thatKey, src);
				if (thisToThat != 0) {
					return ascending ? -thisToThat : thisToThat;
				}
			}
		} catch (FisherException fe) {
			throw new RuntimeException(fe);
		}
		return 0;
	}

	public Internal_SortableTh(Thing payload, Thing[] keys, BoolTh[] ascending, Syntax src) {
		super();
		this.payload = payload;
		this.keys = keys;
		this.ascending = ascending;
		this.src = src;
	}

	@Override
	public Thing invokeMethod(String methodName, Thing[] args,  Syntax src)
			throws FisherException {
		return this.tryThingMethods(methodName, args, src);
	}


//	public Internal_Success invokePat(Id patId, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
//	throws FisherException {
//		return this.patNotUnderstood(patId, args, evaller, frame, src);
//	}

}
