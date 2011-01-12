
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

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;

import fisher.eval.Computer;
import fisher.eval.Evaller;
import fisher.eval.interfaces.Fieldiferous;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.syn.core.Syntax;
import fisher.util.DangerLevel;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  ThingExtended extends Thing implements Fieldiferous  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	public static  boolean AVOID_STUPID_DOOM = false; 

	public static void reset() {
//		methods.clear();
//		ctors.clear();
//		fields.clear();
		srcPerThread = new ThreadLocal<Syntax>();
	}

	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		return false;
	}

	@Override
	public String typeString() {
		return this.getClass().toString();
	}

	public StringTh str() {
		return StringTh.of(this.toString());
	}

	public BoolTh eq(Thing other) {
		return BoolTh.of(this.equals(other));
	}

	public IntTh th_hashcode() {
		return IntTh.of(this.hashCode());
	}

	
	//public static ThreadLocal<Map<Class, Map<String, Map<Integer, Method>>>> methodsPerThread = new ThreadLocal<Map<Class,Map<String,Map<Integer,Method>>>>();
	
	public static Map<Class, Map<String, Map<Integer, Method>>> methods() {
//		Map<Class, Map<String, Map<Integer, Method>>> methods = methodsPerThread.get();
//		if (methods != null) return methods;
//		methods = new HashMap<Class, Map<String,Map<Integer,Method>>>();
//		methodsPerThread.set(methods);
		return methods;
	}
	
//	public static ThreadLocal<Map<Class, Map<Integer, Constructor>>> ctorsPerThread = new ThreadLocal<Map<Class,Map<Integer,Constructor>>>();
	
	public static Map<Class, Map<Integer, Constructor>> ctors() {
//		Map<Class, Map<Integer, Constructor>> ctors = ctorsPerThread.get();
//		if (ctors != null) return ctors;
//		ctors = new HashMap<Class, Map<Integer, Constructor>>();
//		ctorsPerThread.set(ctors);
		return ctors;
	}
	
//	public static ThreadLocal<Map<Class, Map<String, Field>>> fieldsPerThread = new ThreadLocal<Map<Class, Map<String, Field>>>();
	
	public static Map<Class, Map<String, Field>> fields() {
//		Map<Class, Map<String, Field>> fields = fieldsPerThread.get();
//		if (fields != null) return fields;
//		fields = new HashMap<Class, Map<String, Field>>();
//		fieldsPerThread.set(fields);
		return fields;
	}
	
	public static Map<Class, Map<String, Map<Integer, Method>>> methods = new HashMap<Class, Map<String, Map<Integer, Method>>>();
	public static Map<Class, Map<Integer, Constructor>> ctors = new HashMap<Class, Map<Integer, Constructor>>();
	public static Map<Class, Map<String, Field>> fields = new HashMap<Class, Map<String, Field>>();

	public static void addField(Class cls, String name, Field field, Syntax src) {
		Map<String, Field> m2;
		if (fields().containsKey(cls)) {
			m2 = fields().get(cls);
		} else {
			m2 = new HashMap<String, Field>();
			fields().put(cls, m2);
		}
		if (m2.containsKey(name)) {
			Field alreadyThere = m2.get(name);
			if (alreadyThere != field) {
				if (AVOID_STUPID_DOOM) {
					src.flag(DangerLevel.ERROR, "Multiple definitions of field for " + cls + " with name " + name, "");
				}
			}
		} else {
			m2.put(name, field);
			//			System.err.println("ThingExtended.addCtor(" + cls + ", " + arity + ") := " + ctor);
		}
	}

	public static void addConstructor(Class cls, int arity, Constructor ctor, Syntax src) {
		Map<Integer, Constructor> m2;
		final Map<Class, Map<Integer, Constructor>> ctors = ctors();
		if (ctors.containsKey(cls)) {
			m2 = ctors.get(cls);
		} else {
			m2 = new HashMap<Integer, Constructor>();
			ctors.put(cls, m2);
		}
		if (m2.containsKey(arity)) {
			Constructor alreadyThere = m2.get(arity);
			if (alreadyThere != ctor) {
				if (AVOID_STUPID_DOOM) {
					src.flag(DangerLevel.ERROR, "Multiple definitions of constructor for " + cls + " with arity "
							+ arity, "");
				}
			}
		} else {
			m2.put(arity, ctor);
			//			System.err.println("ThingExtended.addCtor(" + cls + ", " + arity + ") := " + ctor);
		}
	}

	// This would be so much easier with Thornly tables.
	public static void addMethod(Class cls, String thornName, int arity, Method method, Syntax src, boolean builtin) {
		Map<String, Map<Integer, Method>> m2;
		if (methods().containsKey(cls)) {
			m2 = methods().get(cls);
		} else {
			m2 = new HashMap<String, Map<Integer, Method>>();
			methods().put(cls, m2);
		}
		Map<Integer, Method> m3;
		if (m2.containsKey(thornName)) {
			m3 = m2.get(thornName);
		} else {
			m3 = new HashMap<Integer, Method>();
			m2.put(thornName, m3);
		}
		if (m3.containsKey(arity)) {
			Method alreadyThere = m3.get(arity);
			if (alreadyThere != method) {
				if (AVOID_STUPID_DOOM && !builtin) {
					src.flag(DangerLevel.ERROR, "Multiple definitions of method named " + thornName + "/" + arity
							+ " for class " + cls, "");
				}
			} else {
				// putting the same thing there twice is harmless
			}
		} else {
			m3.put(arity, method);
			//			System.err.println("ThingExtended.addMethod(" + cls + ", " + name + "/" + arity + "):=" + method);
		}
	}

	public static Constructor getCtor(Class cls, int arity) {
		final Map<Class, Map<Integer, Constructor>> ctors = ctors();
		if (ctors.containsKey(cls)) {
			Map<Integer, Constructor> m2 = ctors.get(cls);
			if (m2.containsKey(arity)) {
				return m2.get(arity);
			} else
				return null;
		} else {
			return null;
		}
	}

	public static Method getMethod(Class cls, String name, int arity) {
		final Map<Class, Map<String, Map<Integer, Method>>> methods = methods();
		if (methods.containsKey(cls)) {
			Map<String, Map<Integer, Method>> m2 = methods.get(cls);
			if (m2.containsKey(name)) {
				Map<Integer, Method> m3 = m2.get(name);
				if (m3.containsKey(arity)) {
					return m3.get(arity);
				} else
					return null;
			} else
				return null;
		} else {
			return null;
		}
	}

	public static Field getFieldRefl(Class cls, String fieldName) {
		final Map<Class, Map<String, Field>> fields = fields();
		if (fields.containsKey(cls)) {
			Map<String, Field> m2 = fields.get(cls);
			return m2.get(fieldName);
		} else
			return null;
	}

	public boolean hasField(String fieldName) {
		Field field = getFieldRefl(this.getClass(), fieldName);
		return field != null;
	}

	public Thing getField(String fieldName, Syntax src) throws FisherException {

		Field fld = getFieldRefl(this.getClass(), fieldName);
		if (fld == null) {
			Thing fromMeth = invokeMethod(fieldName, Computer.NO_ARGS, src);
			return fromMeth;
		}
		try {
			Object o = fld.get(this);
			return (Thing) o;
		} catch (IllegalArgumentException e) {
			Doom.runtime("Illegal argument to field-getting from Java: " + e, src, this);
		} catch (IllegalAccessException e) {
			Doom.runtime("Illegal access to field-getting from Java: " + e, src, this);
		}
		return null;
	}

	@Override
	public Thing invokeMethod(String methodName, Thing[] args,  Syntax src)
			throws FisherException {
		Method implMeth = getMethod(this.getClass(), methodName, args.length);
		Evaller evaller = Evaller.mine();
		Framelike frame = this.frameForInvoke();
		Map<Class, Map<String, Map<Integer, Method>>> mmm = ThingExtended.methods();
		if (implMeth == null) {
			Doom.runtime("No method found in extension class: " + methodName + "/" + args.length, src);
			return null;
		} else {
			Syntax oldSrc = mySrc();
			try {
				setSrc(src);
				Thing thing = (Thing) implMeth.invoke(this, (Object[]) args);
				return thing;
			} catch (IllegalArgumentException e) {
				e.printStackTrace();
				Doom.runtime("Invocation error in extension class: " + e, src);
			} catch (IllegalAccessException e) {
				e.printStackTrace();
				Doom.runtime("Illegal access error in extension class: " + e, src);
			} catch (InvocationTargetException e) {
				e.printStackTrace();
				Doom.runtime("Invocation target exception in extension class: " + e, src);
			} finally {
				setSrc(oldSrc);
			}
			return null;
		}
	}

	public static ThreadLocal<Syntax> srcPerThread = new ThreadLocal<Syntax>();

	public static void setSrc(Syntax src) {
		srcPerThread.set(src);
	}

	public static Syntax mySrc() {
		return srcPerThread.get();
	}

}
