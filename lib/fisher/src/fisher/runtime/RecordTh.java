
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

import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.Frame;
import fisher.eval.interfaces.Fieldiferous;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.auxil.ColInfo;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.syn.RecordField;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  RecordTh extends ThingImmutable implements Fieldiferous  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public static final RecordTh EMPTY = RecordTh.makeReally(null, Collections.EMPTY_LIST, Collections.EMPTY_LIST);

	@Override
	public String typeString() {
		// TODO Auto-generated method stub
		return "record";
	}

	private RecordTh() {
	}
	
	@Override
	public boolean isRecord() {
		return true;
	}
	
	@Override
	public RecordTh asRecord(Syntax src) throws FisherException {
			return this;
	}
	
	
	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		if (ic != null) ic.see(this);
		for (Thing fieldVal : fields.values() ) {
			if (! EvalUtil.isImmutable(fieldVal, ic)) return false; 
		}
		return true;
	}
	
	@Override
	public boolean canBeKey() {
		for (Thing field : fields.values()) {
			if (! EvalUtil.canBeKey(field)) return false;					
		}
		return true;
	}

	public static RecordTh constructJoin(ColInfo[] keyInfos, Thing[] values, ColInfo[] nonkeyInfos,
			Fieldiferous restComesFrom, Syntax src) throws FisherException {
		RecordTh rec = new RecordTh();
		// values may be longer than keyInfos.
		for (int i = 0; i < keyInfos.length; i++) {
			rec.setField(keyInfos[i].name, values[i]);
		}
		if (nonkeyInfos != null)
			for (int i = 0; i < nonkeyInfos.length; i++) {
				ColInfo ci = nonkeyInfos[i];
				Thing thing = restComesFrom.getField(ci.name, src);
				rec.setField(ci.name, thing);
			}
		rec.finish();
		return rec;
	}

	public static RecordTh construct(ColInfo[] colInfos, Fieldiferous ff, Syntax src) throws FisherException {
		RecordTh rec = new RecordTh();
		for (int i = 0; i < colInfos.length; i++) {
			ColInfo ci = colInfos[i];
			Thing thing = ff.getField(ci.name, src);
			rec.setField(ci.name, thing);
		}
		rec.finish();
		return rec;
	}
	public static RecordTh constructForOrd(ColInfo[] colInfos, Fieldiferous ff, Syntax src) throws FisherException {
		RecordTh rec = new RecordTh();
		for (int i = 0; i < colInfos.length; i++) {
			ColInfo ci = colInfos[i];
			Thing thing = ff.getField(ci.name, src);
			rec.setField(ci.name, thing);
		}
		rec.finish();
		return rec;
	}

	public static RecordTh construct(List<RecordField> fields, Framelike frame) throws FisherException {
		RecordTh rec = new RecordTh();
		Evaller evaller = Evaller.mine();
		for (RecordField recordField : fields) {
			String fieldName = recordField.id.str();
			Thing value = evaller.eval(recordField.exp, frame);
			rec.setField(fieldName, value);
		}
		rec.finish();
		return rec;
	}
	
	public static RecordTh recordForMapAssign(ColInfo[] colInfos, Syntax src, String field1, Thing val1, ColInfo[] keyInfos, Thing[] keyVals) throws FisherException {
		RecordTh rec = new RecordTh();
		for (ColInfo keyInfo : keyInfos) {
			rec.setField(keyInfo.name, keyVals[keyInfo.pos]);
		}
		for (ColInfo colInfo : colInfos) {
			String name = colInfo.name;
			if (!name.equals(field1)) rec.setField(name, (Thing)null);
		}
		rec.setField(field1, val1);
		rec.finish();
		return rec;
	}
	
	public static RecordTh recordForOrdMapAssign(ColInfo[] colInfos, Syntax src, String field1, Thing val1) throws FisherException {
		RecordTh rec = new RecordTh();
		for (ColInfo colInfo : colInfos) {
			String name = colInfo.name;
			if (!name.equals(field1)) rec.setField(name, (Thing)null);
		}
		rec.setField(field1, val1);
		rec.finish();
		return rec;
	}
	
	public static RecordTh makeFromRow(ColInfo[] colInfos, Thing[] values) {
		if (colInfos.length != values.length) {
			assert(colInfos.length == values.length); // grackle, gotta set breakpoint on /this/ assertion.
		}
		return makeFromSomeColumns(colInfos, values);
	}
	
	public static RecordTh makeFromSomeColumns(ColInfo[] keyInfos, Thing[] values) {
		RecordTh rec = new RecordTh();
		for(int i = 0; i < keyInfos.length; i++) {
			try {
				rec.setField( keyInfos[i].name, values[keyInfos[i].pos]);
			} catch (FisherException e) {
				Doom.internalCatastrophe("Missing column in table row", null, "keys = " + Bard.sep(keyInfos, ","), 
						"values = " + Bard.sep(values, ","));
			}
		}
		rec.finish();
		return rec;
	}

	public static RecordTh makeReally(Syntax src, List<String> fieldNames, List<? extends Object> stuff) {
		try {
			return make(src, fieldNames, stuff);
		}
		catch (FisherException e) {
			return null;
		}
	}
	public static RecordTh make(Syntax src, List<String> fieldNames, List<? extends Object> stuff) throws FisherException {
		if (fieldNames.size() != stuff.size()) {
			Doom.internal("Wrong number of fields in an internal interpreter function", src, fieldNames, Bard.sep(stuff, "+"));
		}
		RecordTh rec = new RecordTh();
		for (int i = 0; i < fieldNames.size(); i++) {
			rec.setField(fieldNames.get(i), EvalUtil.thingify(stuff.get(i), src));
		}
		rec.finish();
		return rec;
		
	}
	public static RecordTh make(Syntax src, String[] fieldNames, Thing... values) throws FisherException {
		if (fieldNames.length != values.length) {
			Doom.internal("Wrong number of fields in an internal interpreter function", src, fieldNames, values);
		}
		RecordTh rec = new RecordTh();
		for (int i = 0; i < fieldNames.length; i++) {
			rec.setField(fieldNames[i], values[i]);
		}
		rec.finish();
		return rec;
	}

	/**
	 * @return an unfinished RecordTh. Fill in its fields -- which you can't do
	 *         from outside, since setField is private, so you'll have to
	 *         provide a setFieldOfUnfinishedRecord. When you're done building
	 *         it, call finish() on it.
	 */
	public static RecordTh unfinished() {
		return new RecordTh();
	}

	public Map<String, Thing> fields = new HashMap<String, Thing>();
	
	public Iterator<Map.Entry<String, Thing>> fieldIterator() {
		return fields.entrySet().iterator();
	}
	
	public Set<Map.Entry<String,Thing>> fieldSet() {
		return fields.entrySet();
	}
	
	private boolean finished = false;

	public Thing getField(String fieldName, Syntax src) throws FisherException {
		if (fields.containsKey(fieldName)) {
			return fields.get(fieldName);
		}
		else if (methodCode.containsKey(fieldName)) {
			return this.invokeMethod(fieldName, EvalUtil.NO_ARGS,   src);
		}
		else {
		
			Doom.runtime("Not a field of this record :" + fieldName, src, fieldName, this);
			return null;
		}
	}

	public RecordTh except(String fieldName, Thing value) throws FisherException {
		RecordTh u = unfinished();
		// Copy this to u...
		for (Map.Entry<String, Thing> flds : this.fields.entrySet()) {
			u.setField(flds.getKey(), flds.getValue());
		}
		// And modify that field.
		u.setField(fieldName, value);
		u.finish();
		return u;
	}

	public boolean hasField(String fieldName) {
		return fields.containsKey(fieldName);
	}

	public void setField(String fieldName, Thing value) throws FisherException {
		if (finished) {
			Doom.internal("Attempt to set a field of a finished record", null, this, fieldName, value);
		} else {
			fields.put(fieldName, value);
		}
	}

	public void finish() {
		this.finished = true;
	}

	private String[] sortedFields = null;

	private void sortAndCacheFields() {
		if (sortedFields != null && finished)
			return;
		sortedFields = new String[fields.size()];
		fields.keySet().toArray(sortedFields); // unsorted
		Arrays.sort(sortedFields, new Comparator<String>() {
			public int compare(String o1, String o2) {
				return o1.compareTo(o2);
			}
		});
	}

	public String toString() {
		sortAndCacheFields();
		StringBuffer sb = new StringBuffer();
		sb.append("{:");
		for (int i = 0; i < sortedFields.length; i++) {
			sb.append(sortedFields[i] + ":" + fields.get(sortedFields[i]));
			if (i < sortedFields.length - 1)
				sb.append(", ");
		}
		sb.append(":}");
		return sb.toString();
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof RecordTh) {
			RecordTh other = (RecordTh) obj;
			this.sortAndCacheFields();
			other.sortAndCacheFields();
			if (this.sortedFields.length != other.sortedFields.length)
				return false;
			// same size, so...
			for (int i = 0; i < this.sortedFields.length; i++) {
				if (!(this.sortedFields[i].equals(other.sortedFields[i])))
					return false;
			}
			// same field names, so...
			for (int i = 0; i < this.sortedFields.length; i++) {
				String fieldName = this.sortedFields[i];
				Thing thisF = this.fields.get(fieldName);
				Thing otherF = other.fields.get(fieldName);
				try {
					if (!EvalUtil.eq(thisF, otherF))
						return false;
				} catch (FisherException e) {
					throw new RuntimeException(e);
				}
			}
			return true;
		} else
			return false;
	}

	@Override
	public int hashCode() {
		this.sortAndCacheFields();
		int hc = 0;
		for (int i = 0; i < this.sortedFields.length; i++) {
			String f = this.sortedFields[i];
			if (f != null) hc ^= f.hashCode();
			Thing thing = fields.get(f);
			if (thing != null)
				hc ^= (i + thing.hashCode());
		}
		return hc;
	}

	private final static int STR = 0;
	private final static int NUM = 1;
	private final static int ChashCode = 2;
	private final static int CtoMap = 3;
	private static Map<String, Integer> methodCode = new HashMap<String, Integer>();
	static {
		methodCode.put("str", STR);
		methodCode.put("num", NUM);
		methodCode.put("hashCode", ChashCode);
		methodCode.put("toMap", CtoMap);
	}

	@Override
	// WARNING: Nonstandard method invocation protocol!
	public Thing invokeMethod(String methodName, Thing[] args,   Syntax src)
			throws FisherException {
		if (methodCode.containsKey(methodName)) {
			Integer methodC = methodCode.get(methodName);
			Evaller evaller = Evaller.mine();
			Framelike frame = this.frameForInvoke();
			switch (methodC) {
			case STR:
				return this.str();
			case NUM:
				return this.tryThingMethods(methodName, args,    src);
			case ChashCode : return  this.ChashCode(methodName, args, evaller, frame, src); 
			case CtoMap : return  this.CtoMap(methodName, args, evaller, frame, src); 
			default:
				Doom.internal("Method key structure broken", src, methodC);
				return null;
			}
		}
		// This stuff is nonstandard:
		return this.getFieldForNullaryMethod(methodName, args, Evaller.mine(), this.frameForInvoke(), src);
		// return this.methodNotUnderstood(methodName, args, evaller, frame, src);
	}
	
	private Thing getFieldForNullaryMethod (String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		if (args.length == 0) {
			return this.getField(methodName, src);
		}
		else return this.tryThingMethods(methodName, args,   src);
	}
	
	
	private static String[] mapFieldNames = new String[]{TableTh.k.name, TableTh.v.name};
	protected Thing CtoMap(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		TableTh map = TableTh.plainMap(frame);
		for (Map.Entry<String, Thing> kv : this.fields.entrySet()) {
			Thing k = StringTh.of(kv.getKey());
			Thing v = kv.getValue();
			// TODO: man, this does a lot of useless allocation to insert a row into that table!
			RecordTh kvrec = RecordTh.make(src, mapFieldNames, k,v);
			map.addRow(kvrec, src);
		}
		return map;
	}
	
	public Iterable<Thing> iterableFields() {
		return fields.values();
	}


}
