
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

import java.io.File;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import fisher.eval.interfaces.Framelike;
import fisher.runtime.*;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.runtime.auxil.Methodical;
import fisher.runtime.auxil.Typelike;
import fisher.syn.Cmd;
import fisher.syn.Formals;
import fisher.syn.FunBody;
import fisher.syn.MonoBody;
import fisher.syn.TypeConstraint;
import fisher.syn.TypeConstraints;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;
import fisher.util.SpecialCharacters;

public  class  EvalUtil  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public static final Thing[] NO_ARGS = new Thing[0]; 
	
	public static final boolean FANCY_NIP = false;

	public static Object unthingify(Thing th, boolean convertListToArray) {
		if (th == null) return null;
		else if (!convertListToArray && th instanceof ListTh) return th;
		else return th.unthingify();
	}
	
	public static Object[] unthingifyAll(Thing[] things, boolean convertListToArray) {
		Object[] objects = new Object[things.length];
		for (int i = 0; i < objects.length; i++) {
			objects[i] = EvalUtil.unthingify(things[i], convertListToArray);
		}
		return objects;
	}
	
	/**
	 * @param fieldName
	 * @param obj
	 * @return the field of obj named fieldName, or null if that doesn't work (no such field, or not an ObjectTh)
	 */
	public static Thing getFieldOfObjectTh_or_null(String fieldName, Thing obj) {
		if (obj instanceof ObjectTh) {
			ObjectTh o = (ObjectTh) obj;
			try {
				Thing fld = o.getField(fieldName, Evaller.lastSyntax());
				return fld;
			} catch (FisherException e) {
				return null;
			}
		}
		else return null;
	}
	
	/**
	 * @param o
	 * @return a Thing version of o if that makes sense, or a non-Thing if o can't be turned into a Thing.
	 */
	public static Object thingify_or_crap(Object o) {
		if (o == null)
			return null;
		if (o instanceof Thing) return (Thing)o;
		if (o instanceof Long) {
			Long l = (Long) o;
			return IntTh.of(l);
		}
		if (o instanceof Integer) {
			Integer i = (Integer) o;
			return IntTh.of(i);
		}
		if (o instanceof String) {
			String s = (String) o;
			final StringTh of = StringTh.of(s);
//			System.err.println("YakYuk: string " + s + " => " + of );
			return of;
		}
		if (o instanceof Boolean) {
			Boolean b = (Boolean) o;
			return BoolTh.of(b);
		}
		if (o instanceof Double) {
			Double d = (Double) o;
			return FloatTh.of(d);
		}
		if (o instanceof Object[]) {
			Object[] oa = (Object[]) o;
			ListTh L = ListTh.EMPTY;
			for(int i = oa.length-1; i >= 0; i--) {
				Object Li = thingify_or_crap(oa[i]);
				if (Li instanceof Thing) {
					Thing Thli = (Thing) Li;
					L = L.cons(Thli);
				}
				else {
					return Li;
				}
				
			}
			return L;
		}
		if (o instanceof List) {
			List lo = (List) o;
			List<Thing> la = new ArrayList<Thing>(lo.size());
			for(Object xo : lo) {
				int los = lo.size();
				int las = la.size();
				Object xa = thingify_or_crap(xo);
				if (xa instanceof Thing) {
					Thing ta = (Thing) xa;
					la.add(ta);
				}
				else {
					return xa;
				}
			}
			return ListTh.fromJavaList(la);
		}
		if (o instanceof Map) {
			Map mo = (Map) o;
			List<String> fieldNames = Bard.list();
			List<Thing> thungValues = Bard.list();
			for (Object ome : mo.entrySet()) {
				if (ome instanceof Map.Entry) {
					Map.Entry me = (Map.Entry) ome;
					final String key = me.getKey().toString();
					fieldNames.add( key );
					Object v = me.getValue();
					Object ov = thingify_or_crap(v);
					if (ov instanceof Thing) {
						Thing thov = (Thing) ov;
						thungValues.add(thov);
					}
					else {
						return "Can't convert value at key " + key + " which is " + v + " of class " + v.getClass(); 
					}
				}
				else {
					return "Oh dear -- non-map-entry in entrySet? " + ome + " of class " + ome.getClass();
				}
			}
			try {
				RecordTh R = RecordTh.make(null, fieldNames, thungValues);
				return R;
			} catch (FisherException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				return "Error thinging a map: " + e;
			}
		}
		return "Can't convert " + o + " which is a " + o.getClass();
	}
	
	/**
	 * @param o
	 * @return a Thing version of o, or null if it can't be converted.
	 */
	public static Thing thingify(Object o) {
		Object to = thingify_or_crap(o);
		if (to instanceof Thing) {
			Thing tho = (Thing) to;
			return tho;
		}
		else if (to == null) return null;
		else return StringTh.of("(an unthingifable " + o.getClass() + " which looks like " + o.toString() + ")");
	}
	
	public static Thing thingify(Object o, Syntax src) throws FisherException {
		Object c = thingify_or_crap(o);
		if (c instanceof Thing) {
			Thing tc = (Thing) c;
			return tc;
		}
		else if (c == null) return null;
		Doom.runtime("I don't know how to convert " + o + " to a Thing", src, o, "Message: " + c);
		return null;
	}

	public static Thing instantiate(ClassDynamicTh classDyn, Thing[] args, Frame frame, Evaller evaller, Syntax src)
			throws FisherException {
		return classDyn.apply(args, frame, evaller, src);
	}

	private static Map<Syntax, StringTh> nipCache = new HashMap<Syntax, StringTh>();
	public static final StringTh NOTHING_IN_PARTICULAR = null;

	public static StringTh nip(Syntax syn) {
		if (EvalUtil.FANCY_NIP) {
			if (nipCache.containsKey(syn)) {
				return nipCache.get(syn);
			} else {
				String nip = "NothingInParticular(" + syn + ")";
				StringTh nipth = StringTh.of(nip);
				nipCache.put(syn, nipth);
				return nipth;
			}
		} else
			return NOTHING_IN_PARTICULAR;
	}

	public static boolean eq(Thing a, Thing b) throws FisherException {
		if (a == null)
			return (b == null);
		if (b == null)
			return false; // since a != null here
		return a.equals(b);
	}
	
	public static final boolean canBeKey(Thing a) {
		return a == null || a.canBeKey();
	}

	public static final int EQUAL = 0;
	public static final int LESS = -1;
	public static final int MORE = +1;

	public static long compare(Thing a, Thing b, Syntax src) throws FisherException {
		if (a == null && b == null)
			return EQUAL;
		if (a == null && b != null)
			return LESS;
		if (a != null && b == null)
			return MORE;
		if (a.isLong() && b.isLong()) {
			return a.asLong(src) - b.asLong(src);
		}
		if (EvalUtil.eq(a, b))
			return EQUAL;
		// fallback: string comparison of print rep, to give some kind of ordering.
		String sa = a.toString();
		String sb = b.toString();
		int ct = sa.compareTo(sb);
		// Sometimes two values have the same string rep, despite being !=.  
		// When they don't, do something reasonable.
		if (ct != EQUAL) return ct;
		// When they do, do LESS.
		return LESS; 
	}

	public static boolean allLongs(Thing[] things) {
		for (Thing thing : things) {
			if (thing == null || !(thing.isLong()))
				return false;
		}
		return true;
	}
	public static boolean allNumbers(Thing[] things) {
		for (Thing thing : things) {
			if (thing == null || !(thing.isNumber()))
				return false;
		}
		return true;
	}
	
	
	public static boolean allStrings(Thing[] things) {
		for (Thing thing : things) {
			if (thing == null || !(thing.isString()))
				return false;
		}
		return true;
	}
	
	public static boolean anyString(Thing[] things) {
		for (Thing thing : things) {
			if (thing != null && thing.isString()) return true;
		}
		return false;
	}
	
	public static String toString(Thing thing) {
		if (thing == null) return SpecialCharacters.LEEP + "null" + SpecialCharacters.REEP;
		else return thing.toString();
	}
	
	public static byte[] asBytes(Thing thing, Syntax src) throws FisherException {
		if (thing == null) {Doom.runtime("null is not bytes", src); return null;}
		else return thing.asBytes(src);
	}
	
	
	public static long[] asLongs(Thing[] things, Syntax syn) throws FisherException {
		long[] longs = new long[things.length];
		for (int i = 0; i < longs.length; i++) {
			longs[i] = asLong(things[i],syn);
		}
		return longs;
	}
	
	public static long asLong(Thing thing, Syntax syn) throws FisherException {
		if (thing == null){
			Doom.runtime("Can't convert null to a number", syn);
			return 0;
		}
		else {
			return thing.asLong(syn);
		}
	}

	public static double[] asDoubles(Thing[] things, Syntax syn) throws FisherException {
		double[] doubles = new double[things.length];
		for (int i = 0; i < doubles.length; i++) {
			doubles[i] = things[i].asDouble(syn);
		}
		return doubles;
	}

	public static Thing applyFailed(Applyable applyable, Syntax src, Thing[] args, Framelike frame, Evaller evaller)
			throws FisherException {
		Doom.runtime("This cannot be applied.  Perhaps it is not a function; perhaps no argument clause matches; perhaps it has the wrong number of arguments."
				+"\nFunction " + "type="+ EvalUtil.kind((Thing)applyable) 
				+ "\nFunction value = " + EvalUtil.toString((Thing)applyable)
				+ "\nArgs = " + Bard.sep(args, ", ")
				+ "\n"
				, src, "--- applyable: ", applyable, "--- src", src, "--- args", args,
				Bard.sep(args, "\n"), "--- frame", frame, "--- evaller", evaller);
		return Thing.NULL;
	}

	public static Thing methodCall(Thing receiver, Id methodId, Thing[] args,  
			Syntax src) throws FisherException {
		if (receiver == null) {
			return Doom.runtime("Can't call methods on null", src, receiver, methodId, args);
		} else  {
			return receiver.invokeMethod(methodId, args,   src);
		} 
	}
	public static Thing methodCall(Thing receiver, String methodName, Thing[] args, 
			Syntax src) throws FisherException {
		if (receiver == null) {
			return Doom.runtime("Can't call methods on null", src, receiver, methodName, args );
		} else  {
			return receiver.invokeMethod(methodName, args,  src);
		} 
	}

	/**
	 * @param funbody
	 * @param args
	 * @param evaller
	 * @param frameToBindIn
	 * @param src
	 * @return the body of the first monobody whose formals match args, and
	 *         mutates frameToBindIn to add bindings of those formals. Exception
	 *         if no match found.
	 * @throws FisherException
	 */
	public static Cmd findAndBind(FunBody funbody, Thing[] args, Evaller evaller, Frame frameToBindIn, Syntax src)
			throws FisherException {
		for (MonoBody monobody : funbody.funbodies) {
			Formals formals = monobody.formals;
			if (Matchiste.matchFormals(formals, args, frameToBindIn)) {
				// This one! Note -- bindings done by matchFormals;
				return monobody.body;
			}
		}
		Doom.runtime("No matching method body found for method.", src, funbody, args, evaller, frameToBindIn, src);
		return null;
	}
	
	public static Thing enplus(Thing x) {
		if (x == null) return Nullity.of(1);
		else return x.enplus();
	}
	
	static final StringTh STR_NULL = StringTh.of("null");
	public static StringTh kind(Thing x) {
		if (x == null) return STR_NULL;
		else return StringTh.of(x.typeString());
	}
	
	public static boolean isImmutable(Thing thing, ImmutabilityContext ic) {
		if (thing == null) return true;
		else return thing.isImmutable(ic);
	}
	
	public static boolean isPure(Frameable fr) {
		if (fr == null) return true;
		if (fr instanceof Thing) {
			Thing th = (Thing) fr;
			return th.isImmutable(null);
		} else {
			// not a Thing -- such are always impure. 
			return false; 
		}
	}
	
	public static boolean thingIsType(Thing thing, Frameable typeish, Syntax src) throws FisherException{
		if (typeish instanceof Typelike) {
			Typelike cls = (Typelike) typeish;
			return cls.hasInstance(thing);
		}
		
		else {
			Doom.runtime("Not a type: " + typeish + " -- a " + typeish.getClass(), src, "thing = "+ thing, "typeish is a " + typeish.getClass());
			return false;
		}
	}
	
	public static List<Thing>  computeTypes(Framelike framelike, Syntax src, List<Id> types) throws FisherException {
		if (types.isEmpty()) return Collections.EMPTY_LIST;
		List<Thing> t = new ArrayList<Thing>(types.size());
		for (Id type : types) {
			Frameable fr = framelike.baseValue(type.seal(), src);
			if (fr != null) {
				t.add(fr.Rvalue());
			}
		}
		return t;
	}
	
	public static void confirmTypes(Thing thing, List<Thing> types, Syntax src) throws FisherException {
		if (types == null) return;
		if (thing == null) return;
		for (Thing type : types) {
			if (! EvalUtil.thingIsType(thing, type, src)) {
				Doom.runtime("Wrong type: " + src + " is declared to always be " + type
						+ " but here I am trying to make it have value " + thing + " which is a "
						+ thing.typeString() + " and not a " + type + " at all.", src, thing);
			}
		}
	}
	
	public static boolean isIter(Thing thing) {
		if (thing == null) return false;
		try {
			return null != iter(thing,null);
		}
		catch(FisherException fe) {
			return false;
		}
	}
	
	public static Iterable<Thing> iter(Thing thing, Syntax src) throws FisherException {
		if (thing == null) Doom.runtime("Not iterable:" + thing, src);
		return thing.asIter(src);
	}
	
	public static RecordTh recordPlease(Thing betterBeARecord) throws FisherException {
		if (betterBeARecord == null) return null;
		if (betterBeARecord instanceof RecordTh ) return (RecordTh) betterBeARecord;
		Doom.runtime("Record wanted here", Evaller.mine().lastSyntax(), "This is what I got: " + betterBeARecord);
		return null;
	}
	
	public static StringTh stringPlease(Thing stringifyThis) {
		if (stringifyThis == null) return null;
		if (stringifyThis instanceof StringTh) return (StringTh) stringifyThis;
		return StringTh.of(stringifyThis.toString());
	}
	
	public static File asJavaFile(Thing x) throws FisherException {
		if (x instanceof FileTh) {
			FileTh fx = (FileTh) x;
			return fx.file;			
		}
		else if (x == null) {
			return null;			
		}
		else if (x.isString()) {
			return new File(x.toString());
		}
		else return null;
			
	}

}
