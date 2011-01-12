/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 

package fisher.runtime.lib.json;

import java.io.StringReader;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.ingest.Ingester;
import fisher.parser.FisherParser;
import fisher.parser.json.JsonParser;
import fisher.parser.json.ParseException;
import fisher.runtime.Applyable;
import fisher.runtime.BoolTh;
import fisher.runtime.FloatTh;
import fisher.runtime.IntTh;
import fisher.runtime.JavalyFunImpl;
import fisher.runtime.ListTh;
import fisher.runtime.ObjectTh;
import fisher.runtime.RecordTh;
import fisher.runtime.StringTh;
import fisher.runtime.Thing;
import fisher.statics.ClassStatic;
import fisher.statics.Seal;
import fisher.syn.core.Syntax;
import fisher.syn.interfaces.Classlike;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;
import fisher.util.FisherSource;

public class JSON {
	static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
	
	private static JsonParser jsonParser = null;
	
	public static Thing parse(Thing th) throws FisherException {
		
		String s = EvalUtil.toString(th);
		StringReader sr = new StringReader(s);
		if (jsonParser == null) {
			jsonParser = new JsonParser(sr);
		}
		else {
			JsonParser.ReInit(sr);
		}
		try {
			Thing res = JsonParser.JSON();
			return res;
		} catch (Throwable e) {
			//Doom.runtime("JSON parse error: \n" + e, Evaller.lastSyntax());
			return null;
		}
	}
	

	private static boolean isJSON(Thing thing, Set<Thing> visited)  {
		if (thing == null) return true;
		if (visited.contains(thing)) {
			// If we see something that's not JSON, we immediately return false altogether.
			// So, if we've seen something before, we think it's json (or looks like it so far)
			// and we don't need to explore it further
			return true;
		}
		if (thing instanceof ObjectTh) return false;
		if (thing instanceof IntTh || thing instanceof StringTh ||  thing instanceof BoolTh  || thing instanceof FloatTh ) return true;
		if (thing  instanceof ListTh) {
			ListTh L = (ListTh) thing ;
			visited.add(thing);
			for(Thing el : L) {
				if (! isJSON(el, visited)) return false;
			}
			return true;			
		}
		if (thing instanceof RecordTh) {
			RecordTh R = (RecordTh) thing;
			visited.add(R);
			for(Thing el : R.iterableFields()) {
				if (! isJSON(el, visited)) return false;
			}
			return true;
			}			
		return false;
	}
	
	public static Thing isJSON(Thing x) throws FisherException {
		final boolean b = JSON.isJSON(x, new HashSet<Thing>(1));
		return BoolTh.of(b);
	}
	
	public static Thing asJSON(Thing thing) throws FisherException {
		return asJSON(thing, null);
	}
	public static Thing asJSON(Thing thing, Thing fun) throws FisherException {
		if (thing == null) return null;
		if (thing instanceof IntTh || thing instanceof StringTh ||  thing instanceof BoolTh  || thing instanceof FloatTh ) return thing;
		if (thing  instanceof ListTh) {
			ListTh L = (ListTh) thing ;
			List<Thing> res = new ArrayList<Thing>(L.length);
			for(Thing el : L) {
				res.add(asJSON(el, fun));
			}
			return ListTh.fromJavaList(res);			
		}
		if (thing instanceof RecordTh) {
			RecordTh R = (RecordTh) thing;
			RecordTh res = RecordTh.unfinished();
			for(Map.Entry<String, Thing> ent : R.fieldSet()) {
				res.setField(ent.getKey(), asJSON(ent.getValue(), fun));
			}
			res.finish();
			return res;
			}			
		if (fun == null) Doom.runtime("asJSON: trying to convert a " + EvalUtil.kind(thing) + " but no converter function is supplied",
				Evaller.lastSyntax(), thing);
		if (! (fun instanceof Applyable)) 
			Doom.runtime("asJSON: conversion function must be a function, not " + EvalUtil.kind(fun), Evaller.lastSyntax(), 
					"thing=" + thing, "\nfun=" + fun);
		Applyable afun = (Applyable) fun;
		Thing res = afun.apply(Bard.array(thing), null, Evaller.mine(), Evaller.lastSyntax());
		if (!isJSON(res, new HashSet<Thing>(0))) 
			Doom.runtime("asJSON: conversion function must return a JSON-compliant value.", Evaller.lastSyntax(), 
					"argument=" + thing, "result=" + res);
		return res;
	}
	
	
	private final static JavalyFunImpl jsonRecordizeFunImpl = 
		JavalyFunImpl.notDeclaredFromThorn(Ingester.seekThingMethod(JSON.class, "jsonRecordize", 1, null), 1);
	
	public static Thing jsonRecordize(Thing thing) throws FisherException {
		if (thing instanceof ObjectTh) {
			ObjectTh obj = (ObjectTh) thing;
			RecordTh res = RecordTh.unfinished();			
			for (String fieldName : obj.fields.keySet()) {
				Thing fieldVal = obj.getField(fieldName, null);
				Thing jsonnedFieldVal = asJSON(fieldVal, jsonRecordizeFunImpl);
				res.setField(fieldName, jsonnedFieldVal);
			}
			res.finish();
			return res;			
		}
		else return asJSON(thing, jsonRecordizeFunImpl);
	}
	
	private final static JavalyFunImpl jsonRecordizeWithClassNameFunImpl = 
		JavalyFunImpl.notDeclaredFromThorn(Ingester.seekThingMethod(JSON.class, "jsonRecordize", 1, null), 1);
	
	public static Thing jsonRecordizeWithClassName(Thing thing) throws FisherException {
		if (thing instanceof ObjectTh) {
			ObjectTh obj = (ObjectTh) thing;
			RecordTh res = RecordTh.unfinished();			
			for (String fieldName : obj.fields.keySet()) {
				Thing fieldVal = obj.getField(fieldName, null);
				Thing jsonnedFieldVal = asJSON(fieldVal, jsonRecordizeWithClassNameFunImpl);
				res.setField(fieldName, jsonnedFieldVal);
			}
			String className = obj.classDynamic.toString();
			res.setField("class", StringTh.of(className));
			res.finish();
			return res;			
		}
		else return asJSON(thing, jsonRecordizeWithClassNameFunImpl);
	}
	

}
