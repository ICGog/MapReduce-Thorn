
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

import java.util.Collections;
import java.util.ConcurrentModificationException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.interfaces.Fieldiferous;
import fisher.eval.interfaces.Framelike;
import fisher.syn.Table;
import fisher.syn.TypeConstraints;
import fisher.syn.core.ColAccess;
import fisher.syn.core.ColSpecial;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;
import fisher.util.FisherRuntimeException;
import fisher.runtime.auxil.*;

public  class  TableTh extends AbstractTableTh implements Applyable, Iterable<RecordTh>  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public final Table tableSrc;

	public final Set<Row> rowSet = new HashSet<Row>();

	private boolean forbidMapSet = false;
	private boolean forbidInvalidMapAccess = false;

	private final Map rootMap = new HashMap();
	
	public static final ColInfo k = new ColInfo("k", ColAccess.KEY, ColSpecial.NORMAL, 0, null, new TypeConstraints(null, null, Collections.EMPTY_LIST));
	public static final ColInfo v = new ColInfo("v", ColAccess.VAR, ColSpecial.MAP, 1, null, new TypeConstraints(null, null, Collections.EMPTY_LIST));
	
	public static final ColInfo[] MAP_COL_INFOS = new ColInfo[]{k,v};
	public static final ColInfo[] MAP_KEY_INFOS = new ColInfo[]{k};
	public static final ColInfo[] MAP_NONKEY_INFOS = new ColInfo[]{v};
	
	public static TableTh plainMap(Framelike framelike) throws FisherException{
		return new TableTh(MAP_COL_INFOS, MAP_KEY_INFOS, MAP_NONKEY_INFOS, v, null, framelike);
	}
		
		

//	public TableTh(ColInfo[] colInfos, ColInfo[] keyInfos, ColInfo[] nonkeyInfos, ColInfo mapColInfo, Table tableSrc) {
//		super();
//		this.colInfos = colInfos;
//		this.keyInfos = keyInfos;
//		this.tableSrc = tableSrc;
//		this.nonkeyInfos = nonkeyInfos;
//		this.mapColInfo = mapColInfo;
//	}
	
	public String toString() {
		return "{" + Bard.sep(rowSet, ", ") + "}";
	}
	
	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		return false;
	}

	public TableTh(ColInfo[] colInfos, ColInfo[] keyInfos, ColInfo[] nonkeyInfos, ColInfo mapColInfo, Table tableSrc, Framelike framelike) throws FisherException {
		super(colInfos, keyInfos, nonkeyInfos, mapColInfo);
		this.tableSrc = tableSrc;
		storeTypeConstraints(colInfos, tableSrc, framelike);
	}



	

	public ColInfo colInfoFor(String colName, Syntax src) throws FisherException {
		for (int i = 0; i < colInfos.length; i++) {
			ColInfo ci = colInfos[i];
			if (ci.name.equals(colName))
				return ci;
		}
		Doom.runtime("No column named " + colName, src, this);
		return null;
	}

	public void clear() {
		rowSet.clear();
		rootMap.clear();
		assert (this.computeInvariant());

	}

	private Map finalMap(Fieldiferous ff, Syntax src, boolean createIntermediates, boolean missingIsNull, boolean confirmKeyNess)
			throws FisherException {
		Map cursor = rootMap;
		Syntax oldAt = Doom.oldAt(src);
		for (int i = 0; i < keyInfos.length - 1; i++) {
			String colName = keyInfos[i].name;
			if (ff.hasField(colName)) {
				Thing f = ff.getField(colName, src);
				confirmKeyNess(src, confirmKeyNess, f);
				if (cursor.containsKey(f)) {
					cursor = (Map) cursor.get(f);
				} else if (createIntermediates) {
					Map newMap = new HashMap();
					cursor.put(f, newMap);
					cursor = newMap;
				} else if (missingIsNull) {
					return null;
				} else {
					Doom.runtime("Table doesn't have member " + ff + "; specifically field " + colName
							+ " has no entry for " + f + " here.", src, this, ff);
				}
			} else {
				Doom.runtime("Table subscript doesn't have a required key field (" + colName + ").", src, ff);
			}
		}
		Doom.at(oldAt);
		return cursor;
	}

	private void confirmKeyNess(Syntax src, boolean confirmKeyNess, Thing f)
			throws FisherException {
		if (confirmKeyNess) {
			if (! EvalUtil.canBeKey(f) ) {
				Doom.runtime("Can't be a key: " + f, src, this);
			}
		}
	}

	private Map finalMap(Thing[] keys, Syntax src, boolean createIntermediates, boolean missingIsNull)
			throws FisherException {
		if (keys.length < this.keyInfos.length) {
			Doom.runtime("Wrong number of subscripts", src, keys, this);
		}
		// But keys can be longer; this method ignores those elements.
		// That feature is used in ():=
		Syntax oldAt = Doom.oldAt(src);
		Map cursor = rootMap;
		for (int i = 0; i < keyInfos.length - 1; i++) {
			Thing f = keys[i];
			confirmKeyNess(src, true, f);
			if (cursor.containsKey(f)) {
				cursor = (Map) cursor.get(f);
			} else if (createIntermediates) {
				Map newMap = new HashMap();
				cursor.put(f, newMap);
				cursor = newMap;
			} else if (missingIsNull) {
				return null;
			} else {
				Doom.runtime("Table doesn't have a row with keys= " + Bard.sep(keys, ", "), src);
			}
		}
		Doom.at(oldAt);
		return cursor;
	}

	public void addRow(Fieldiferous ff, Syntax src) throws FisherException {
		Map finalMap = finalMap(ff, src, true, false, true);
		Syntax oldAt = Doom.oldAt(src);
		String finalColName = keyInfos[keyInfos.length - 1].name;
		if (ff.hasField(finalColName)) {
			Thing f = ff.getField(finalColName, src);
			confirmKeyNess(src, true, f);
			boolean hadARecordThere = finalMap.containsKey(f);
			Row row = Row.of(this, ff, src);
			confirmFieldiferousHasRightTypes(ff, src);
			if (hadARecordThere) {
				Row oldrow = (Row) finalMap.get(f);
				rowSet.remove(oldrow);
				// Replacing extant row, so don't change num.
			}
			finalMap.put(f, row);
			rowSet.add(row);
		} else {
			Doom.runtime("Table subscript doesn't have a required key field (" + finalColName + ").", src, ff);
		}
		Doom.at(oldAt);
	}

	private Thing CassignToMap(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		if (forbidMapSet) {
			Doom.runtime("Map assignments m[k]:=v have been disabled for this map.  Perhaps it has other columns that need to be set too", src);
		}
		if (mapColInfo == null) {
			Doom.runtime("Not a map", src, this);
		}
		if (args.length != keyInfos.length + 1) {
			Doom.runtime("Wrong number of subscripts", src);
		}
		mutateField(args, mapColInfo.name, args[args.length - 1], src, 1, true);
		return null;
	}

	private Thing CassignSubscripted(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		Thing assignee = args[args.length - 1];
		if (assignee == null) {
			this.delRowMinusOne(args, src);
			return null;
		}
		if (!(assignee instanceof Fieldiferous)) {
			Doom
					.runtime(
							"Can't assign this as the non-key fields of a table row.  It must be a record or object (or use map operations).",
							src, assignee, this);
		}
		Fieldiferous ff = (Fieldiferous) assignee;
		RecordTh rec = RecordTh.constructJoin(keyInfos, args, nonkeyInfos, ff, src);
		this.addRow(rec, src);
		return null;
	}

	public void delRowMinusOne(Thing[] keysPlusOne, Syntax src) throws FisherException {
		Map finalMap = finalMap(keysPlusOne, src, false, true);
		if (finalMap == null) {
			return; // it wasn't there in the first place
		}
		Thing finalKey = keysPlusOne[keyInfos.length - 1];
		internalDelRow(finalMap, finalKey);
	}

	public void mutateField(Thing[] subscripts, String fieldName, Thing value, Syntax src, int nExtraKeys,
			boolean makeNewNullRowIfRowMissing) throws FisherException {
		Syntax oldAt = Doom.oldAt(src);
		ColInfo fieldInfo = colInfoFor(fieldName, src);
		EvalUtil.confirmTypes(value, this.typeConstraints.get(fieldInfo), src);
		if (fieldInfo.access != ColAccess.VAR) {
			Doom.runtime("Can't assign to field " + fieldName + ", because it is not 'var'.", src, this, subscripts);
		}
		Row oldRow = getRow(subscripts, src, nExtraKeys);
		if (oldRow == null) {
			if (!makeNewNullRowIfRowMissing) {
				Doom.runtime("Can't mutate field " + fieldName + " of a row that doesn't exist ("
						+ Bard.sep(subscripts, ",") + ").", src, subscripts, this, value);
			}
			else {
				// Make a row that's all nulls except for the given field.
				RecordTh newRow = RecordTh.recordForMapAssign(nonkeyInfos, src, fieldName, value, keyInfos, subscripts);
				this.addRow(newRow, src);
			}
		} else {
			oldRow.mutateField(fieldInfo, value);
		}
		Doom.at(oldAt);
	}

	public void delRow(Fieldiferous ff, Syntax src) throws FisherException {
		Syntax oldAt = Doom.oldAt(src);
		Map finalMap = finalMap(ff, src, false, true, false);
		if (finalMap == null) {
			return; // it wasn't there in the first place
		}
		String finalColName = keyInfos[keyInfos.length - 1].name;
		if (ff.hasField(finalColName)) {
			Thing f = ff.getField(finalColName, src);
			internalDelRow(finalMap, f);
		} else {
			Doom.runtime("Table subscript doesn't have a required key field (" + finalColName + ").", src, ff);
		}
		Doom.at(oldAt);
	}

	private void internalDelRow(Map finalMap, Thing f) {
		boolean hadARecordThere = finalMap.containsKey(f);
		if (hadARecordThere) {
			Row oldrow = (Row) finalMap.get(f);
			rowSet.remove(oldrow);
			finalMap.remove(f);
		} else {
			// It's not there, so no need to do more work.
		}
	}

	public RecordTh recordToAdd(Fieldiferous ff, Syntax src) throws FisherException {
		return RecordTh.construct(this.colInfos, ff, src);
	}

	public RecordTh getRowAsRecord(Fieldiferous ff, Syntax src) throws FisherException {
		Row row = getRow(ff, src);
		if (row == null) return null;
		return row.asRecordTh();
	}
	public Row getRow(Fieldiferous ff, Syntax src) throws FisherException {
		Syntax oldAt = Doom.oldAt(src);
		Map finalMap = finalMap(ff, src, false, true, false);
		if (finalMap == null)
			return null;

		String finalColName = keyInfos[keyInfos.length - 1].name;
		if (ff.hasField(finalColName)) {
			Thing f = ff.getField(finalColName, src);
			Doom.at(oldAt);
			boolean hadARecordThere = finalMap.containsKey(f);
			if (hadARecordThere) {
				Row row = (Row) finalMap.get(f);
				return row;
			} else {
				return null;
			}
		} else {
			Doom.at(oldAt);
			Doom.runtime("Table subscript doesn't have a required key field (" + finalColName + ").", src, ff);
			return null;
		}

	}

	public Row getRow(Thing[] keys, Syntax src, int nExtraKeys) throws FisherException {
		int nKeysProvided = keys.length;
		int nKeysNeeded = keyInfos.length + nExtraKeys;
		if (nKeysProvided != nKeysNeeded) {
			Doom.runtime("Wrong number of keys for table.  Wanted " + nKeysNeeded + " but got " + nKeysProvided, src);
		}
		Map finalMap = finalMap(keys, src, false, true);
		if (finalMap == null)
			return null;
		Syntax oldAt = Doom.oldAt(src);
		Thing f = keys[keyInfos.length - 1];
		boolean hadARecordThere = finalMap.containsKey(f);
		Doom.at(oldAt);
		if (hadARecordThere) {
			Row row = (Row) finalMap.get(f);
			return row;
		} else
			return null;
	}

	@Override
	public Iterable<Thing> asIter(Syntax src) throws FisherException {
		return new Iterable<Thing>(){
			public Iterator<Thing> iterator() {
				return new Iterator<Thing>() {
					private Iterator<RecordTh> recIter = TableTh.this.iterator();
					public Thing next() {
						return recIter.next();
					}
					public boolean hasNext() {
						return recIter.hasNext();
					}
					public void remove() {
						recIter.remove();
					}
				};
			}
		};
	}
	
	public Iterator<RecordTh> iterator() {
		return new Iterator<RecordTh>() {
			private Iterator<Row> rowIt = rowSet.iterator();
			public RecordTh next() {
				try {
					Row row = rowIt.next();
					return row == null ? null : row.asRecordTh();
				} catch (ConcurrentModificationException e) {
					Doom.runtimeNonThorn("You can't modify a table while iterating over it", Evaller.lastSyntax(), this);
					return null;
				}
			}
			public boolean hasNext() {
				return rowIt.hasNext();
			}
			public void remove() {
				rowIt.remove();
			}
		};
	}

	private void fillInRow(Thing[] row, Fieldiferous ff, Syntax src) throws FisherException {
		for (int i = 0; i < colInfos.length; i++) {
			ColInfo colInfo = colInfos[i];
			row[i] = ff.getField(colInfo.name, src);
		}
	}

	public Thing apply(Thing[] args, Framelike frame, Evaller evaller, Syntax src) throws FisherException {
		Row row = this.getRow(args, src, 0);
		return row == null ? null : row.asRecordTh();
	}

	private final static int STR = 0;
	private final static int NUM = 1;
	private final static int Cins = 2;
	private final static int Cget = 3;
	private final static int Cdel = 4;
	private final static int Cclear = 5;
	private final static int CtstCheckInvariant = 6;
	private final static int CassignSubscripted = 7;
	private final static int Crows = 8;
	private final static int CinsNew = 9;
	private final static int CdelOld = 10;
	private final static int Ckeys = 11;
	private final static int Cmapget = 12;
	private final static int CforbidInvalidMapAccess = 13;
	private final static int CassignToMap = 14;
	private final static int CforbidMapSet = 15;
	private final static int ChashCode = 16;
	private final static int CtoRecord = 17;
	private final static int CinsAll = 18;
	private final static int CanyRow = 19;
	private final static int Csame = 20;
	private final static int Chas = 21;

	private static Map<String, Integer> methodCode = new HashMap<String, Integer>();
	static {
		methodCode.put("str", STR);
		methodCode.put("num", NUM);
		methodCode.put("len", NUM);
		//methodCode.put("@=", Cins);
		methodCode.put("ins", Cins);
		methodCode.put("ins!", Cins);
		methodCode.put("get", Cget);
		//methodCode.put("\\=", Cdel);
		methodCode.put("del", Cdel);
		methodCode.put("del!", Cdel);
		methodCode.put("clear!", Cclear);
		methodCode.put("tstCheckInvariant", CtstCheckInvariant);
		methodCode.put("():=", CassignSubscripted);
		methodCode.put("rows", Crows);
		methodCode.put("insNew", CinsNew);
		methodCode.put("insNew!", CinsNew);
		methodCode.put("delOld", CdelOld);
		methodCode.put("delOld!", CdelOld);
		methodCode.put("keys", Ckeys);	
		methodCode.put("[]", Cmapget);
		methodCode.put("forbidInvalidMapAccess", CforbidInvalidMapAccess);
		methodCode.put("[]:=", CassignToMap);
		methodCode.put("forbidMapSet", CforbidMapSet);
		methodCode.put("hashCode", ChashCode);
		methodCode.put("toRecord", CtoRecord);
		methodCode.put("insAll", CinsAll);
		methodCode.put("insAll!", CinsAll);
		methodCode.put("anyRow", CanyRow);
		methodCode.put("same?", Csame);
		methodCode.put("has?", Chas);
	}

	@Override
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
				return this.Cnum(methodName, args, evaller, frame, src);
			case Cins:
				return this.Cins(methodName, args, evaller, frame, src);
			case Cget:
				return this.Cget(methodName, args, evaller, frame, src);
			case Cdel:
				return this.Cdel(methodName, args, evaller, frame, src);
			case CtstCheckInvariant:
				return this.CtstCheckInvariant(methodName, args, evaller, frame, src);
			case Cclear:
				return this.Cclear(methodName, args, evaller, frame, src);
			case CassignSubscripted:
				return this.CassignSubscripted(methodName, args, evaller, frame, src);
			case Crows:
				return this.Crows(methodName, args, evaller, frame, src);
			case CinsNew:
				return this.CinsNew(methodName, args, evaller, frame, src);
			case CdelOld:
				return this.CdelOld(methodName, args, evaller, frame, src);
			case Ckeys:
				return this.Ckeys(methodName, args, evaller, frame, src);
			case Cmapget:
				return this.Cmapget(methodName, args, evaller, frame, src);
			case CforbidInvalidMapAccess:
				return this.CforbidInvalidMapAccess(methodName, args, evaller, frame, src);
			case CassignToMap:
				return this.CassignToMap(methodName, args, evaller, frame, src);
			case CforbidMapSet:
				return this.CforbidMapSet(methodName, args, evaller, frame, src);
			case ChashCode:
				return this.ChashCode(methodName, args, evaller, frame, src);
			case CtoRecord:
				return this.CtoRecord(methodName, args, evaller, frame, src);
			case CinsAll:
				return this.CinsAll(methodName, args, evaller, frame, src);
			case CanyRow:
				return this.CanyRow(methodName, args, evaller, frame, src);
			case Csame:
				return this.Csame(methodName, args, evaller, frame, src);
			case Chas:
				return this.Chas(methodName, args, evaller, frame, src);
			default:
				Doom.internal("Method key structure broken", src, methodC);
				return null;
			}
		}
		return this.tryThingMethods(methodName, args,   src);
	}

	private BoolTh Chas(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		Thing t = args[0];
		if (this.keyInfos.length != 1) Doom.runtime("Wrong number of keys for 'has?'", src);
		final Row row = getRow(args, src, 0);
		return BoolTh.of(row != null);
	}
	private IntTh Cnum(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		return IntTh.of(rowSet.size());
	}

	private Thing Cclear(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		this.clear();
		return null;
	}

	private Thing CanyRow(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		for (Row row : this.rowSet) {
			return row.asRecordTh();
		}
		return null;
	}
	private Thing Crows(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		ListTh L = rows();
		return L;
	}



	private ListTh rows() {
		ListTh L = ListTh.EMPTY;
		for (Row row : this.rowSet) {
			L = L.cons(row.asRecordTh());
		}
		return L;
	}
	
	private Set<RecordTh> recordSet() {
		Set<RecordTh> recset = new HashSet<RecordTh>(this.rowSet.size());
		for (Row row : this.rowSet) {
			recset.add(row.asRecordTh());
		}
		return recset;
	}

	private Thing Ckeys(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		ListTh L = ListTh.EMPTY;
		for (Row row : this.rowSet) {
			RecordTh keys = row.keyRecord(); 
			L = L.cons(keys);
		}
		return L;
	}

	private Thing CtstCheckInvariant(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		boolean same = computeInvariant();
		return BoolTh.of(same);
	}

	private boolean computeInvariant() {
		Set<Row> invSet = new HashSet<Row>();
		// Calculate invSet from hash tables.
		invFillIn(invSet, rootMap);
		boolean same = invSet.equals(rowSet);
		return same;
	}

	private void invFillIn(Set<Row> invSet, Map map) {
		for (Object o : map.values()) {
			if (o instanceof Map) {
				Map mo = (Map) o;
				invFillIn(invSet, mo);
			} else {
				invSet.add((Row) o);
			}
		}
	}

	private Thing CinsAll(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		for (Thing thing : EvalUtil.iter(args[0], src)) {
			addThing(src, thing);
		}
//		Thing stuff = args[0];
//		if (stuff instanceof Iterable) {
//			Iterable istuff = (Iterable) stuff;
//			for (Iterator iterator = istuff.iterator(); iterator.hasNext();) {
//				Thing thing = (Thing) iterator.next();
//				addThing(src, thing);
//			}
//		}
		return null;
	}
	
	private RecordTh Cget(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		if (args[0] instanceof Fieldiferous) {
			Fieldiferous row = (Fieldiferous) args[0];
			RecordTh r = this.getRowAsRecord(row, src);
			return r;
		} else {
			Doom.runtime("'get' requires a record or object.", src, args[0]);
			return null;
		}
	}
	
	private Thing CforbidInvalidMapAccess(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		boolean b = args[0].asBoolean(src);
		this.forbidInvalidMapAccess = b;
		return null;
	}
	private Thing CtoRecord(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		if(keyInfos.length != 1) Doom.runtime(".toRecord() only works on tables with a single key", src, this);
		if(nonkeyInfos.length != 1) Doom.runtime(".toRecord() only works on tables with a single non-key", src, this);
		RecordTh r = RecordTh.unfinished();
		int ik = keyInfos[0].pos;
		int iv = nonkeyInfos[0].pos;
		for (Row row : rowSet) {
			Thing k = row.values[ik];
			Thing v = row.values[iv];
			String s = k.toString();
			if (r.hasField(s)) {
				Doom.runtime("Two keys in this table have .str() = '"+s+"', so the table cannot be made into a record.", src, this, k, v);
			}
			r.setField(s, v);
		}
		r.finish();
		return r;
		
	}

	private Thing CforbidMapSet(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		boolean b = args[0].asBoolean(src);
		this.forbidMapSet = b;
		return null;
	}

	private Thing Cmapget(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		if (mapColInfo == null) {
			Doom.throwy("Not a map", src);
		}
		Row wholeRow = this.getRow(args, src, 0);
		if (wholeRow == null) {
			if (forbidInvalidMapAccess) {
				Doom.throwy("Not a key", src);
			} else {
				return null;
			}
		}
		Thing mapField = wholeRow.values[mapColInfo.pos];
		return mapField;
	}

	private Thing Cdel(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		if (args[0] instanceof Fieldiferous) {
			Fieldiferous row = (Fieldiferous) args[0];
			this.delRow(row, src);
			return null;
		} else {
			Doom.runtime("Can't figure out what keys you want me to delete. Please supply a record or object.", src,
					args[0]);
			return null;
		}
	}

	private Thing Csame(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		return BoolTh.of(args[0] == this);
	}
	private Thing CdelOld(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		if (args[0] instanceof Fieldiferous) {
			Fieldiferous row = (Fieldiferous) args[0];
			Row therement = getRow(row, src);
			if (therement == null) {
				Doom.throwy("delOld: row not there!", src);
			}
			this.delRow(row, src);
			return null;
		} else {
			Doom.runtime("Can't figure out what keys you want me to delete. Please supply a record or object.", src,
					args[0]);
			return null;
		}
	}

	private AbstractTableTh CinsNew(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		if (args[0] instanceof Fieldiferous) {
			Fieldiferous row = (Fieldiferous) args[0];
			if (getRow(row, src) != null) {
				Doom.throwy("insNew: No! Table already has row!", src);
			}
			this.addRow(row, src);
			return null;
		} else {
			Doom.runtime("Can't insert anything without fields in a table.", src, args[0]);
			return null;
		}
	}

	private AbstractTableTh Cins(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		final Thing thing = args[0];
		return addThing(src, thing);
	}



	private AbstractTableTh addThing(Syntax src, final Thing thing) throws FisherException, FisherRuntimeException {
		if (thing instanceof Fieldiferous) {
			Fieldiferous row = (Fieldiferous) thing;
			this.addRow(row, src);
			return null;
		} else {
			Doom.runtime("Can't insert anything without fields in a table.", src, thing);
			return null;
		}
	}

	@Override
	public String typeString() {
		// TODO Auto-generated method stub
		return "table";
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null) return false;
		if(obj.getClass() == TableTh.class) {
			TableTh tother = (TableTh) obj;
			final Set<String> thiscol = this.columnNameSet();
			final Set<String> othercol = tother.columnNameSet();
			if(!(thiscol.equals(othercol))) {
				return false;
			}
			
			final Set<RecordTh> thir = this.recordSet();
			final Set<RecordTh> othr = tother.recordSet();
			boolean eq = thir.equals(othr);
			return	eq;
		}
		else return false;
	}
	
	private Set<String> columnNameSet() {
		Set<String> cns = new HashSet<String>(this.colInfos.length);
		for (ColInfo col : this.colInfos) {
			cns.add(col.name);
		}
		return cns;
	}
	
	
}
