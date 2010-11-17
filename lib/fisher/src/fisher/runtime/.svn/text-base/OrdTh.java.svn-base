
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

import java.util.ArrayList;
import java.util.ConcurrentModificationException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.auxil.ColInfo;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.syn.Ord;
import fisher.syn.core.ColAccess;
import fisher.syn.core.Syntax;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  OrdTh extends AbstractTableTh implements Iterable<Thing>, Applyable  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	private final Ord ordSrc;

	private boolean forbidMapSet = false;

	private List<RecordTh> elements = new ArrayList<RecordTh>(0);

	public OrdTh(ColInfo[] colInfos, ColInfo[] keyInfos, ColInfo[] nonkeyInfos, ColInfo mapColInfo, Ord ordSrc, Framelike framelike) throws FisherException {
		super(colInfos, keyInfos, nonkeyInfos, mapColInfo);
		this.ordSrc = ordSrc;
		storeTypeConstraints(colInfos, ordSrc, framelike);
	}

	@Override
	public String typeString() {
		return "ord";	
	}
	
	
	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		return false;
	}
	
	@Override
	public Iterable<Thing> asIter(Syntax src) throws FisherException {
		return this;
	}
	
	public Iterator<Thing> iterator() {
		if (mapColInfo == null) {
			Doom.runtimeNonThorn("Can't iterate over a non-'map' ord,.  Location of error is probably somewhere near the listed line", Evaller.lastSyntax());
		}
		
		return new Iterator<Thing>() {
			Iterator<RecordTh> recit = elements.iterator();

			public Thing next() {
				// TODO Auto-generated method stub
				RecordTh r = recit.next();
				try {
					return r.getField(mapColInfo.name, null);
				} catch (FisherException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
					return null;
				} catch (ConcurrentModificationException e) {
					Doom.runtimeNonThorn("You can't modify a table while iterating over it", Evaller.lastSyntax(), this);
					return null;
				}
			}

			public boolean hasNext() {
				// TODO Auto-generated method stub
				return recit.hasNext();
			}

			public void remove() {
				// TODO Auto-generated method stub
				recit.remove();
			}
		};
	}

	public String toString() {
		return "ord(" + Bard.sep(elements, ", ") + ")";
	}

	private RecordTh recordThatFits(Thing thingThatOughtToBeARecord, String methodName, Evaller evaller, Syntax src)
			throws FisherException {
		if (!thingThatOughtToBeARecord.isRecord()) {
			Doom.runtime("Record needed as argument to " + methodName, src, thingThatOughtToBeARecord);
		}
		RecordTh rec = thingThatOughtToBeARecord.asRecord(src);
		for (ColInfo ci : nonkeyInfos) {
			if (!rec.hasField(ci.name)) {
				Doom.runtime("Record needs field " + ci.name + " to be inserted into this ord.", src, this, rec);
			}
		}
		// OK, it has all the fields.

		RecordTh rr = RecordTh.constructForOrd(nonkeyInfos, rec, src);
		return rr;
	}

	private void addElement(RecordTh record, int ipos, Syntax src) throws FisherException {
		confirmFieldiferousHasRightTypes(record, src);
		elements.add(ipos, record);
	}

	public int pos(long i) {
		int len = this.elements.size();
		if (i > len)
			return len;
		if (i >= 0)
			return (int) i;
		if (i >= -len)
			return (int) len + (int) i;
		return 0;
	}
	
	private void doDelete(long i, Syntax src) throws FisherException {
		int p = pos(i);
		if (p >= elements.size()) {
			Doom.runtime("Index out of bounds in deletion: " + i, src, i, p);
		}
		elements.remove(p);
	}
	
	public RecordTh get(Thing[] subscripts, Syntax src, boolean returnNullIfError) throws FisherException {
		if (subscripts.length != 1) Doom.runtime("Ords take exactly one subscript", src);
		int i = subscripts[0].asJavaInt(src);
		return get(i, src, returnNullIfError);
	}

	public RecordTh get(int i, Syntax src, boolean returnNullIfError) throws FisherException {
		int pi = pos(i);
		assert (pi >= 0);
		if (pi >= elements.size()) {
			if (returnNullIfError) return null;
			else Doom.runtime("Index out of bounds", src, this, i, pi);
		}
		return elements.get(pi);
	}

	public Thing getMapFieldFromActualIndex(int i, Syntax src) throws FisherException {
		RecordTh rec = elements.get(i);
		return rec.getField(mapColInfo.name, src);
	}
	
	private final static int STR = 0;
	private final static int NUM = 1;
	private final static int Cadd = 2;
	private final static int Crows = 3;
	private final static int Cadd1 = 4;
	private final static int Cmapget = 5;
	private final static int Cmapput = 6;
	private final static int CassignSubscripted = 7;
	private final static int Clst = 8;
	private final static int CaddAfter = 9;
	private final static int CaddBefore = 10;
	private final static int Cclear = 11;
	private final static int Cdel = 12;
	private final static int Cpos = 13;
	private final static int CforbidMapSet = 14;
	private final static int ChashCode = 15;
	private final static int CaddAll = 16;
	private final static int Cadd1All = 17;
	private final static int Cremove = 18;
	private final static int Csame = 19;

	private static Map<String, Integer> methodCode = new HashMap<String, Integer>();
	static {
		methodCode.put("str", STR);
		methodCode.put("num", NUM);
		methodCode.put("len", NUM);
		methodCode.put("add", Cadd);
		methodCode.put("add!", Cadd);
		methodCode.put("rows", Crows);
		methodCode.put("@=", Cadd1);
		methodCode.put("[]", Cmapget);
		methodCode.put("[]:=", Cmapput);
		methodCode.put("():=", CassignSubscripted);
		methodCode.put("lst", Clst);
		methodCode.put("addAfter", CaddAfter);
		methodCode.put("addBefore", CaddBefore);
		methodCode.put("clear!", Cclear);
		methodCode.put("del!", Cdel);
		methodCode.put("del", Cdel);
		methodCode.put("pos", Cpos);
		methodCode.put("forbidMapSet", CforbidMapSet);
		methodCode.put("hashCode", ChashCode);
		methodCode.put("addAll!", CaddAll);
		methodCode.put("addAll", CaddAll);
		methodCode.put("add1", Cadd1);
		methodCode.put("add1!", Cadd1);
		methodCode.put("add1All", Cadd1All);
		methodCode.put("add1All!", Cadd1All);
		methodCode.put("remove", Cremove);
		methodCode.put("remove!", Cremove);
		methodCode.put("same?", Csame);
	}

	@Override
	public Thing invokeMethod(String methodName, Thing[] args, Syntax src)
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
			case Cadd:
				return this.Cadd(methodName, args, evaller, frame, src);
			case Crows:
				return this.Crows(methodName, args, evaller, frame, src);
			case Cadd1:
				return this.Cadd1(methodName, args, evaller, frame, src);
			case Cmapget:
				return this.Cmapget(methodName, args, evaller, frame, src);
			case Cmapput:
				return this.Cmapput(methodName, args, evaller, frame, src);
			case CassignSubscripted:
				return this.CassignSubscripted(methodName, args, evaller, frame, src);
			case Clst:
				return this.Clst(methodName, args, evaller, frame, src);
			case CaddAfter:
				return this.CaddAfter(methodName, args, evaller, frame, src);
			case CaddBefore:
				return this.CaddBefore(methodName, args, evaller, frame, src);
			case Cclear:
				return this.Cclear(methodName, args, evaller, frame, src);
			case Cdel:
				return this.Cdel(methodName, args, evaller, frame, src);
			case CforbidMapSet:
				return this.CforbidMapSet(methodName, args, evaller, frame, src);
			case Cpos:
				return this.Cpos(methodName, args, evaller, frame, src);
			case ChashCode:
				return this.ChashCode(methodName, args, evaller, frame, src);
			case CaddAll:
				return this.CaddAll(methodName, args, evaller, frame, src);
			case Cadd1All:
				return this.Cadd1All(methodName, args, evaller, frame, src);
			case Cremove:
				return this.Cremove(methodName, args, evaller, frame, src);
			case Csame:
				return this.Csame(methodName, args, evaller, frame, src);
			default:
				Doom.internal("Method key structure broken", src, methodC);
				return null;
			}
		}
		return this.tryThingMethods(methodName, args, src);
	}

	public Thing apply(Thing[] args, Framelike frame, Evaller evaller, Syntax src) throws FisherException {
		checkNumberOfArgs(1, 1, "()", args, evaller, frame, src);
		Thing subscript = args[0];
		if (subscript == null) {
			Doom.runtime("Subscript cannot be null", src);
			return null;
		} else if (subscript.isLong()) {
			int i = (int) subscript.asLong(src);
			RecordTh rec = get(i, src, true);
			return rec;
		} else {
			Doom.runtime("Subscript to ord is not a number", src, subscript);
			return null;
		}
	}
	
	
	
	private Thing Cclear(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		List<Thing> L = new ArrayList<Thing>();
		this.elements.clear();
		return null;
	}

	
	private Thing Clst(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		List<Thing> L = new ArrayList<Thing>();
		if (mapColInfo == null) {
			Doom.runtime("No map field", src, this);
		}
		for (RecordTh thing : elements) {
			L.add(thing.getField(mapColInfo.name, src));
		}
		return ListTh.fromJavaList(L);
		
	}
	
	private Thing Cremove(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		Thing removee = args[0];
		for(int i = 0; i < elements.size(); i++) {
			Thing mapf = this.getMapFieldFromActualIndex(i, src);
			if (EvalUtil.eq(mapf, removee)) {
				this.doDelete(i, src);
				return null;
			}
		}
		return null;
	} 
	private Thing Csame(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		return BoolTh.of(this == args[0]);
	} 
	
	private Thing Cadd1All(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		if (this.forbidMapSet) {
			Doom.runtime("@= is disabled for this ord.", src, this, args[0]);
		}
//		for (Thing thing : args[0].asList(src)) {
		for (Thing thing : EvalUtil.iter(args[0],src)) {
			RecordTh r = RecordTh.recordForOrdMapAssign(colInfos, src, mapColInfo.name, thing);
			addElement(r, elements.size(), src);
		}
		return null;
	} 
	
	private Thing CaddAll(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		for (Thing thing : EvalUtil.iter(args[0],src)) {
			RecordTh r = recordThatFits(thing, methodName, evaller, src);
			addElement(r, elements.size(), src);
		}
		return null;
	}
	
	private Thing CforbidMapSet(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		Thing subscript = args[0];
		boolean b = subscript.asBoolean(src);
		this.forbidMapSet = b;
		return null;
	}
	
	private Thing Cdel(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		Thing subscript = args[0];
		if (subscript == null) {
			Doom.runtime("Subscript cannot be null", src);
			return null;
		} else if (subscript.isLong()) {
			int i = (int) subscript.asLong(src);
			int p = pos(i);
			if (p >= elements.size()) {
				Doom.runtime("Index out of bounds", src, this, i, p);
				return null;
			}
			else {
				return elements.remove(p);
			}
		}else if (subscript instanceof IntRangeTh) {
			IntRangeTh range = (IntRangeTh) subscript;
			int from = (int) range.min;
			int to = (int) range.max;
			if (from < 0 || to >= elements.size()) {
				Doom.runtime("Range subscript out of range: " + range + " for an ord with len()=" + elements.size(), src, this, range);
				return null;
			}
			else {
				for (int i = to; i >= from; i--) {
					elements.remove(i);
				}
				return null;
			}
			
		} 
		else {
			Doom.runtime("Subscript to del is not a number", src, subscript);
			return null;
		}
	}
	
	private Thing Cmapget(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		Thing subscript = args[0];
		if (subscript == null) {
			Doom.runtime("Subscript cannot be null", src);
			return null;
		} else if (subscript.isLong()) {
			int i = (int) subscript.asLong(src);
			RecordTh rec = get(i, src, false);
			if (mapColInfo == null) {
				Doom.runtime("No map field", src, this);
			}
			return rec.getField(mapColInfo.name, src);
		} else {
			Doom.runtime("Subscript to ord is not a number", src, subscript);
			return null;
		}
	}

	private Thing Cpos(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		Thing subscript = args[0];
		if (subscript == null) {
			Doom.runtime("Subscript cannot be null", src);
			return null;
		} else if (subscript.isLong()) {
			int i = (int) subscript.asLong(src);
			int p = pos(i);
			if (p >= elements.size()) {
				Doom.runtime("Index out of bounds", src, this, i, p);
			}
			return IntTh.of(p);
		} else {
			Doom.runtime("Subscript to pos is not a number", src, subscript);
			return null;
		}
	}
	
	private Thing CaddBefore(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		Thing subscript = args[0];
		Thing newVal = args[1];
		
		if (subscript == null) {
			Doom.runtime("Subscript cannot be null", src);
			return null;
		} else if (subscript.isLong()) {
			int i = (int) subscript.asLong(src);
			RecordTh rec = newVal.asRecord(src);
			RecordTh newRow = recordThatFits(rec, src);
			int p = pos(i);
			if (p == 0) {
				// Always allowed
				elements.add(0, rec);
			}
			else if (p >= elements.size()) {
				elements.add(rec);
			}
			else {
				elements.add(p, rec);
			}
			return null;
		} else {
			Doom.runtime("Subscript to ord is not a number", src, subscript);
			return null;
		}
	}

	private Thing CaddAfter(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		Thing subscript = args[0];
		Thing newVal = args[1];
		
		if (subscript == null) {
			Doom.runtime("Subscript cannot be null", src);
			return null;
		} else if (subscript.isLong()) {
			int i = (int) subscript.asLong(src);
			RecordTh rec = newVal.asRecord(src);
			RecordTh newRow = recordThatFits(rec, src);
			int p = pos(i);
			if (p >= elements.size()) {
				elements.add(rec);
			}
			else {
				elements.add(p+1, rec);
			}
			return null;
		} else {
			Doom.runtime("Subscript to ord is not a number", src, subscript);
			return null;
		}
	}
	public Thing Cmapput(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		Thing subscript = args[0];
		Thing newVal = args[1];
		if (mapColInfo == null) {
			Doom.runtime("No map field", src, this);
		}
		if (this.forbidMapSet) {
			Doom.runtime("Map assignments forbidden for this ord.", src);
			return null;
		}
		if (mapColInfo.access != ColAccess.VAR) {
			Doom.runtime("Map field (" +  mapColInfo.name + ") is not 'var', so it cannot be mutated.", src);
			return null;
		}
		return mutateField(subscript, mapColInfo.name, newVal, src );
	}
	
	public Thing mutateField(Thing subscript, String fieldName, Thing newVal, Syntax src) throws FisherException {
		if (subscript == null) {
			Doom.runtime("Subscript cannot be null", src);
			return null;
		} else if (subscript.isLong()) {
			int i = (int) subscript.asLong(src);
			RecordTh rec = get(i, src, false);
			
			RecordTh mutatedRecord = rec.except(fieldName, newVal);
			elements.set(pos(i), mutatedRecord);
			return null;
		} else {
			Doom.runtime("Subscript to ord is not a number", src, subscript);
			return null;
		}
	}
	
	public Thing mutateField(Thing[] subscripts, String fieldName, Thing newVal, Syntax src, int nExtraKeys,
			boolean makeNewNullRowIfRowMissing) throws FisherException {
		
		if (subscripts.length != 1 + nExtraKeys) {
			Doom.runtime("Ords take only a single subscript", src);
			return null;
		}
		Thing subscript = subscripts[0];
		return mutateField(subscript, fieldName, newVal, src);
	}

	private Thing CassignSubscripted(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		Thing subscript = args[0];
		Thing newVal = args[1];
		if (subscript == null) {
			Doom.runtime("Subscript cannot be null", src);
			return null;
		} else if (subscript.isLong()) {
			int i = (int) subscript.asLong(src);
			if (newVal == null) {
				doDelete(i, src);
				return null;
			} else {
				RecordTh rec = newVal.asRecord(src);
				RecordTh mutatedRecord = recordThatFits(rec, src);
				elements.set(pos(i), mutatedRecord);
				return null;
			}
		} else {
			Doom.runtime("Subscript to ord is not a number", src, subscript);
			return null;
		}
	}

	private Thing Cadd(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		for (Thing thing : args) {
			RecordTh r = recordThatFits(thing, methodName, evaller, src);
			addElement(r, elements.size(), src);
		}
		return null;
	}

	Thing[] NOTHING = new Thing[] {};

	private Thing Cadd1(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		if (this.forbidMapSet) {
			Doom.runtime("@= is disabled for this ord.", src, this, args[0]);
		}
		RecordTh r = RecordTh.recordForOrdMapAssign(colInfos, src, mapColInfo.name, args[0]);
		addElement(r, elements.size(), src);
		return null;
	}

	private Thing Crows(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return ListTh.fromJavaList(elements);
	}

	private IntTh Cnum(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		return IntTh.of(elements.size());
	}

	@Override
	public boolean equals(Object obj) {
		if(obj == null) return false;
		if (obj instanceof OrdTh) {
			OrdTh oth = (OrdTh) obj;
			return this.columnNameSet().equals(oth.columnNameSet()) && elements.equals(oth.elements);
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
