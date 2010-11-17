
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.eval.interfaces;

import java.io.File;
import java.util.List;

import fisher.eval.Evaller;
import fisher.eval.Frame;
import fisher.runtime.Frameable;
import fisher.runtime.ObjectTh;
import fisher.runtime.Thing;
import fisher.runtime.VarCell;
import fisher.statics.Seal;
import fisher.syn.core.Syntax;
import fisher.util.FisherException;

public interface Framelike {
	public Thing RValue(Seal seal, Syntax src) throws FisherException;

	public VarCell LValue(Seal seal, Syntax src) throws FisherException;

	/**
	 * Store (usually bind) f into the <b>top level</b> of this.
	 * 
	 * @param seal
	 * @param f
	 * @param src
	 * @throws FisherException
	 */
	public void store(Seal seal, Frameable f, Syntax src) throws FisherException;

	/**
	 * @param seal
	 * @param src
	 * @return the Frameable for a seal -- a VarCell for a var, a Thing for a
	 *         val. Useful for copying into another frame, say.
	 * @throws FisherException
	 */
	public Frameable baseValue(Seal seal, Syntax src) throws FisherException;

	public Thing fetchImmediateMemberR(Seal seal, Syntax src) throws FisherException;

	public ObjectTh theThis(Evaller evaller, Syntax src) throws FisherException;
	
	public List<Seal> seals();
	
	public boolean hasSeal(Seal seal);
	
	public void setIt(Thing subject, Syntax src) throws FisherException;
	
	public Thing getIt(Syntax src) throws FisherException;
	

}
