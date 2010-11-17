
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.eval;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import fisher.eval.interfaces.Framelike;
import fisher.runtime.Frameable;
import fisher.runtime.ObjectTh;
import fisher.runtime.Thing;
import fisher.runtime.VarCell;
import fisher.runtime.builtInFun.BIFfer;
import fisher.statics.PredefinedIdentifiers;
import fisher.statics.Seal;
import fisher.syn.core.Syntax;
import fisher.util.Doom;
import fisher.util.FisherException;
import fisher.util.FisherRuntimeException;

public  class  Frame implements Framelike  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public final Framelike parent;
	public ObjectTh thThis = null;

	public Thing it = null;

	public static Frame rootFrame() throws FisherException {
		Frame frame = new Frame(null);
		for (PredefinedIdentifiers.Predef pd : PredefinedIdentifiers.predefs) {
			Frameable f = BIFfer.of(pd);
			frame.store(pd.seal, f, null);
		}
		return frame;
	}

	public static Frame inner(Framelike parent) {
		return new Frame(parent);
	}

	private Frame(Framelike parent) {
		this.parent = parent;
	}

	private Map<Seal, Frameable> members = new HashMap<Seal, Frameable>();

	public Thing RValue(Seal seal, Syntax src) throws FisherException {
		if (members.containsKey(seal)) {
			Frameable frameable = members.get(seal);
			if (frameable == null)
				return null;
			Thing res = frameable.Rvalue();
			return res;
		}
		if (this.parent != null) {
			Thing res = parent.RValue(seal, src);
			return res;
		} else {
			System.err.println("Frame.RValue doom: seal = " + seal + "; src = " + src + "; frame=" + members);
			Doom.runtime("Attempt to fetch  " + seal + ", but it has not been set yet.", src, this);
			return null;
		}
	}

	public VarCell LValue(Seal seal, Syntax src) throws FisherException {
		if (members.containsKey(seal)) {
			Frameable frameable = members.get(seal);
			if (frameable instanceof VarCell) {
				VarCell cell = (VarCell) frameable;
				return cell;
			} else {
				Doom.runtime("Attempt to assign to " + seal.str() + ", which is not bound to a VarCell.", src, seal,
						frameable, this);
				return null;
			}

		}
		if (this.parent != null) {
			VarCell res = parent.LValue(seal, src);
			return res;
		} else {
			Doom.runtime("Attempt to assign to " + seal.str() + ", which is not yet bound.", src, seal);
			return null;
		}
	}

	/* (non-Javadoc)
	 * @see fisher.eval.interfaces.Framelike#store(fisher.statics.Seal, fisher.runtime.Frameable, fisher.syn.core.Syntax)
	 * 
	 */
	public void store(Seal seal, Frameable f, Syntax src) throws FisherException {
		if (seal == null) {
			Doom.internal("Attempt to store a null Seal", src, f, members, this);
		}
		seal.check(this, f, src);
		members.put(seal, f);
	}

	public Frameable baseValue(Seal seal, Syntax src) throws FisherException {
		if (members.containsKey(seal)) {
			return members.get(seal);
		} else if (parent != null) {
			Frameable res = parent.baseValue(seal, src);
			return res;
		} else {
			Doom.runtime("Attempt to get " + seal + " out of " + this + " but it's not there.", src, seal, members,
					this);
			return null;
		}
	}

	public String toString() {
		StringBuffer sb = new StringBuffer();
		sb.append("[");
		for (Map.Entry<Seal, Frameable> ent : members.entrySet()) {
			if (!ent.getKey().builtIn) {
				sb.append(ent.getKey() + "=" + ent.getValue() + " ");
			}
		}
		sb.append("]");
		return sb.toString();
	}

	public Thing fetchImmediateMemberR(Seal seal, Syntax src) throws FisherException {
		if (members.containsKey(seal)) {
			Frameable frameable = members.get(seal);
			Thing res = frameable.Rvalue();
			return res;
		} else {
			Doom.runtime("Attempt to fetch  " + seal + ", but it has not been set yet.", src, this);
			return null;
		}
	}

	public boolean hasSeal(Seal seal) {
		return members.containsKey(seal) || (parent != null && parent.hasSeal(seal));
	}

	public List<Seal> seals() {
		List<Seal> S = parent == null ? new ArrayList<Seal>() : parent.seals();
		S.addAll(members.keySet());
		return S;
	}

	public void setThis(ObjectTh that) {
		this.thThis = that;
	}

	public ObjectTh theThis(Evaller evaller, Syntax src) throws FisherException {
		if (thThis != null)
			return thThis;
		else if (this.parent != null)
			return parent.theThis(evaller, src);
		else
			return Doom.noThis(evaller, src);
	}

	public void setIt(Thing subject, Syntax src) throws FisherException {
		this.it = subject;
	}

	public Thing getIt(Syntax src) throws FisherException {
		return this.it;
	}

}
