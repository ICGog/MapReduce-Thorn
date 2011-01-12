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

import java.util.HashSet;
import java.util.Iterator;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.runtime.auxil.Methodical;
import fisher.runtime.lib.json.JSON;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;

/**
 * @author bard All runtime Thorn objects are Things.
 */
public abstract class Thing extends Frameable implements Methodical {
	static String copyright() {
		return fisher.util.Copyright.IBM_COPYRIGHT;
	}

	public static final String INVOKE = "invoke";
	public static final Thing NULL = null;

	@Override
	public Thing Rvalue() {
		return this;
	}

	public abstract String typeString();

	public boolean isLong() {
		return false;
	}

	public boolean isString() {
		return false;
	}

	public boolean isBoolean() {
		return false;
	}

	public boolean isClosure() {
		return false;
	}

	public boolean isList() {
		return false;
	}

	public boolean isRange() {
		return false;
	}

	public boolean isIntRange() {
		return false;
	}

	public boolean isFloat() {
		return false;
	}

	public boolean isNumber() {
		return false;
	}

	public boolean isTable() {
		return false;
	}

	public boolean isRecord() {
		return false;
	}

	public boolean isOrdPos() {
		return false;
	}

	public boolean isBytes() {
		return false;
	}

	public boolean canBeKey() {
		return false;
	}

	public abstract boolean isImmutable(ImmutabilityContext ic);

	@Override
	public boolean transmissible() {
		return this.isImmutable(new ImmutabilityContext());
	}

	public long asLong(Syntax src) throws FisherException {
		Doom.runtime("Not an integer", src, this);
		return -123321;
	}

	public byte[] asBytes(Syntax src) throws FisherException {
		Doom.runtime("Not a bytes", src, this);
		return null;
	}

	public double asDouble(Syntax src) throws FisherException {
		Doom.runtime("Not a number", src, this);
		return -123321;
	}

	public int asJavaInt(Syntax src) throws FisherException {
		return (int) this.asLong(src);
	}

	public String asString(Syntax src) throws FisherException {
		Doom.runtime("Not a string", src, this);
		return null;
	}

	public boolean asBoolean(Syntax src) throws FisherException {
		Doom.internal("Something that the interpreter thought had to be BoolTh, wasn't.  It was " + this
				+ " and was a " + this.typeString(), src, this);
		return true;
	}

	public ListTh asList(Syntax src) throws FisherException {
		return (ListTh) Doom.runtime("Not a list", src, this);
	}

	public final BoolTh asBoolTh(Syntax src) throws FisherException {
		return BoolTh.of(this.asBoolean(src));
	}

	public Iterable<Thing> asIter(Syntax src) throws FisherException {
		Doom.runtime("Not iterable:" + this, src);
		return null;
	}

	public AbstractRangeTh asRange(Syntax src) throws FisherException {
		return (AbstractRangeTh) Doom.runtime("Not a range", src, this);
	}

	public RecordTh asRecord(Syntax src) throws FisherException {
		return (RecordTh) Doom.runtime("Not a record", src, this);
	}

	public IntRangeTh asIntRange(Syntax src) throws FisherException {
		return (IntRangeTh) Doom.runtime("Not an int range", src, this);
	}

	public Thing invokeMethod(String methodName, Thing[] args, Syntax src) throws FisherException {
		return tryThingMethods(methodName, args, src);
	}

	public final Thing invokeMethod(Id methodId, Thing[] args, Syntax src) throws FisherException {
		return this.invokeMethod(methodId.str(), args, src);
	}

	public Thing tryThingMethods(String methodName, Thing[] args, Syntax src) throws FisherException {
		if (Thing.INVOKE.equals(methodName)) {
			return do_invoke(args, src);
		}
		
		return Doom.runtime(this.typeString() + " does not understand method " + methodName + "\nthis = " + this
				+ "\nargs = " + Bard.sep(args, ", "), src, this, args);
	}

	
	
	
	public Thing do_invoke(Thing[] args, Syntax src) throws FisherException {
		checkNumberOfArgs(2, 2, Thing.INVOKE, args, src);
		String methodNameToInvoke = args[0].asString(src);
		ListTh listOfArgs = args[1].asList(src);
		Thing[] argsToInvokeWith = listOfArgs.toJavaArray();
		Thing invoked = this.invokeMethod(methodNameToInvoke, argsToInvokeWith, src);
		return invoked;
	}

	public Framelike frameForInvoke() {
		return Evaller.mine().rootFrame;
	}

	/**
	 * @return a Java object corresponding to this as best as possible.
	 */
	public Object unthingify() {
		return this;
	}

	public Thing deplus() {
		return this;
	}

	public Thing enplus() {
		return this;
	}

	public StringTh str() {
		return StringTh.of(this.toString());
	}

	protected void checkNumberOfArgs(int min, int max, String methodName, Thing[] args, Evaller DUMMYevaller,
			Framelike DUMMYframe, Syntax src) throws FisherException {
		checkNumberOfArgs(min, max, methodName, args, src);
	}

	protected void checkNumberOfArgs(int min, int max, String methodName, Thing[] args, Syntax src)
			throws FisherException {
		if (args.length < min || args.length > max) {
			Doom.runtime("Wrong number of arguments for " + methodName + " -- it needs " + min + ".." + max, src, args);
		}
	}

	protected void checkArg(Thing arg, Class requiredClass, Syntax src) throws FisherException {
		if (requiredClass.isInstance(arg)) {
			return;
		} else {
			Doom.runtime("Wrong type of argument.  Wanted " + requiredClass + " but got " + EvalUtil.kind(arg), src,
					arg);
		}
	}

}
