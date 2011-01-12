/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */

package fisher.runtime.lib;

import fisher.eval.Evaller;
import fisher.runtime.FloatTh;
import fisher.runtime.Thing;
import fisher.util.FisherException;

public class Math {
	static String copyright() {
		return fisher.util.Copyright.IBM_COPYRIGHT;
	}

	public static Thing sqrt(Thing x) throws FisherException {
		double d = x.asDouble(Evaller.lastSyntax());
		return FloatTh.of(java.lang.Math.sqrt(d));
	}

	public static Thing abs(Thing x) throws FisherException {
		double d = x.asDouble(Evaller.lastSyntax());
		return FloatTh.of(java.lang.Math.abs(d));
	}

	public static Thing acos(Thing x) throws FisherException {
		double d = x.asDouble(Evaller.lastSyntax());
		return FloatTh.of(java.lang.Math.acos(d));
	}

	public static Thing asin(Thing x) throws FisherException {
		double d = x.asDouble(Evaller.lastSyntax());
		return FloatTh.of(java.lang.Math.asin(d));
	}

	public static Thing atan(Thing x) throws FisherException {
		double d = x.asDouble(Evaller.lastSyntax());
		return FloatTh.of(java.lang.Math.atan(d));
	}

	public static Thing cbrt(Thing x) throws FisherException {
		double d = x.asDouble(Evaller.lastSyntax());
		return FloatTh.of(java.lang.Math.cbrt(d));
	}

	public static Thing cos(Thing x) throws FisherException {
		double d = x.asDouble(Evaller.lastSyntax());
		return FloatTh.of(java.lang.Math.cos(d));
	}

	public static Thing cosh(Thing x) throws FisherException {
		double d = x.asDouble(Evaller.lastSyntax());
		return FloatTh.of(java.lang.Math.cosh(d));
	}

	public static Thing exp(Thing x) throws FisherException {
		double d = x.asDouble(Evaller.lastSyntax());
		return FloatTh.of(java.lang.Math.exp(d));
	}

	public static Thing expm1(Thing x) throws FisherException {
		double d = x.asDouble(Evaller.lastSyntax());
		return FloatTh.of(java.lang.Math.expm1(d));
	}

	public static Thing floor(Thing x) throws FisherException {
		double d = x.asDouble(Evaller.lastSyntax());
		return FloatTh.of(java.lang.Math.floor(d));
	}

	public static Thing log(Thing x) throws FisherException {
		double d = x.asDouble(Evaller.lastSyntax());
		return FloatTh.of(java.lang.Math.log(d));
	}

	public static Thing log10(Thing x) throws FisherException {
		double d = x.asDouble(Evaller.lastSyntax());
		return FloatTh.of(java.lang.Math.log10(d));
	}

	public static Thing log1p(Thing x) throws FisherException {
		double d = x.asDouble(Evaller.lastSyntax());
		return FloatTh.of(java.lang.Math.log1p(d));
	}

	public static Thing sin(Thing x) throws FisherException {
		double d = x.asDouble(Evaller.lastSyntax());
		return FloatTh.of(java.lang.Math.sin(d));
	}

	public static Thing sinh(Thing x) throws FisherException {
		double d = x.asDouble(Evaller.lastSyntax());
		return FloatTh.of(java.lang.Math.sinh(d));
	}

	public static Thing tan(Thing x) throws FisherException {
		double d = x.asDouble(Evaller.lastSyntax());
		return FloatTh.of(java.lang.Math.tan(d));
	}

	public static Thing tanh(Thing x) throws FisherException {
		double d = x.asDouble(Evaller.lastSyntax());
		return FloatTh.of(java.lang.Math.tanh(d));
	}

	public static Thing ulp(Thing x) throws FisherException {
		double d = x.asDouble(Evaller.lastSyntax());
		return FloatTh.of(java.lang.Math.ulp(d));
	}

}
