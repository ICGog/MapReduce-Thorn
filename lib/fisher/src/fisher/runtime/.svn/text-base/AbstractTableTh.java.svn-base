
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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.interfaces.Fieldiferous;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.auxil.ColInfo;
import fisher.syn.Table;
import fisher.syn.TypeConstraints;
import fisher.syn.core.Syntax;
import fisher.util.Doom;
import fisher.util.FisherException;
import fisher.util.FisherRuntimeException;

public abstract  class  AbstractTableTh extends ThingBuiltIn  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public final ColInfo[] colInfos;
	public final ColInfo[] keyInfos;
	public final ColInfo[] nonkeyInfos;
	public final ColInfo mapColInfo;
	
	public final Map<ColInfo, List<Thing>> typeConstraints = new HashMap<ColInfo, List<Thing>>();

	
	/**
	 * @param rec
	 * @param src
	 * @return true if the fit is perfect, false if it is imperfect but still fits; exception if doesn't fit.
	 * @throws FisherException
	 */
	protected boolean confirmRecordFits(RecordTh rec, Syntax src) throws FisherException {
		confirmFieldiferousHasRightTypes(rec, src);
		return rec.fields.size() == colInfos.length;
	}

	protected void confirmFieldiferousHasRightTypes(Fieldiferous rec, Syntax src) throws FisherRuntimeException,
			FisherException {
		for (ColInfo ci : colInfos) {
			if (! rec.hasField(ci.name)) {
				Doom.runtime("Record needs field " + ci.name + " to fit this table.", src, rec, this);
			}
			if (this.typeConstraints.containsKey(ci)) {
				Thing fi = rec.getField(ci.name, src);
				EvalUtil.confirmTypes(fi, this.typeConstraints.get(ci), src);
			}
			
		}
	}
	
	protected RecordTh recordThatFits(RecordTh rec, Syntax src) throws FisherException {
		if (confirmRecordFits(rec, src)) {
			// Just right.
			return rec;
		}
		else {
			return RecordTh.construct(colInfos, rec, src);
		}
	}

	@Override
	public String typeString() {
		// TODO Auto-generated method stub
		return null;
	}

	public boolean isTable() {
		return true;
	}

	public AbstractTableTh(ColInfo[] colInfos, ColInfo[] keyInfos, ColInfo[] nonkeyInfos, ColInfo mapColInfo) {
		super();
		this.colInfos = colInfos;
		this.keyInfos = keyInfos;
		this.nonkeyInfos = nonkeyInfos;
		this.mapColInfo = mapColInfo;
	}
	
	protected void storeTypeConstraints(ColInfo[] colInfos, Syntax tableSrc, Framelike framelike) throws FisherException {
		for (ColInfo colInfo : colInfos) {
			final TypeConstraints typeConstraints2 = colInfo.typeConstraints;
			List<Thing> tycon = EvalUtil.computeTypes(framelike, tableSrc, typeConstraints2.ids());
			if(!(tycon.isEmpty())) this.typeConstraints.put(colInfo, tycon);
		}
	}

}
