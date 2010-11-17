
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
import java.lang.reflect.Member;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import fisher.eval.Evaller;
import fisher.eval.interfaces.Fieldiferous;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.runtime.auxil.Typelike;
import fisher.syn.QualName;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  ExtensionClass extends Thing implements Applyable, Typelike { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	

	public final Class implClass;
	public final Id thornName;
	public final List<Id> fields;
	public final List<String> fieldNames;
	public final Map<String, Field> reflectFields = new HashMap<String, Field>();
	
	public String toString() {
		return implClass.toString()+"";
	}
	
	




	public ExtensionClass(Class implClass, Id thornName, List<Id> fields) throws FisherException{
		super();
		this.implClass = implClass;
		this.thornName = thornName;
		this.fields = fields;
		this.fieldNames = new ArrayList<String>(fields.size());
		for (Id id2 : fields) {
			fieldNames.add(id2.str());
		}
		try {
			for (Id id : fields) {
				Field field = implClass.getField(id.str());
				reflectFields.put(id.str(), field);
			}
		} catch (SecurityException e) {
			Doom.runtime("Security doom: " + e, null, this);
		} catch (NoSuchFieldException e) {
			Doom.runtime("No Such Field doom: " + e, null, this);
		}
	}






	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public String typeString() {
		// TODO Auto-generated method stub
		return "extension class";
	}

	public Thing apply(Thing[] args, Framelike frame, Evaller evaller, Syntax src) throws FisherException {
		Constructor ctor = ThingExtended.getCtor(implClass, args.length);
		if (ctor == null) {
			Doom.runtime("No " + args.length + "-ary constructor for " + implClass,src,"");
			return null;
		}
		else {
			try {
				Object o = ctor.newInstance((Object[]) args);
				return (Thing) o;
			} catch (IllegalArgumentException e) {
				Doom.runtime("Illegal Argument Exception: " + e, src);
			} catch (InstantiationException e) {
				Doom.runtime("Java Exception: " + e, src);
			} catch (IllegalAccessException e) {
				Doom.runtime("Java Exception: " + e, src);
			} catch (InvocationTargetException e) {
				Doom.runtime("Java Exception: " + e, src);
			}
			return null;
		}
	}

	public boolean hasInstance(Thing thing) {
		return implClass.isInstance(thing);
	}
	
}
