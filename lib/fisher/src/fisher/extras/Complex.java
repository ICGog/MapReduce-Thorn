
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.extras;

import fisher.eval.Evaller;
import fisher.runtime.FloatTh;
import fisher.runtime.Thing;
import fisher.runtime.ThingExtended;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  Complex extends ThingExtended  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	private Thing x, y;
	public Complex(Thing x, Thing y) throws FisherException {
		if (x.isNumber() && y.isNumber()) {
			this.x = x;
			this.y = y;
		}
		else {
			Doom.runtime("Complex numbers need to have real numbers as coordinates.", null, "If you don't like that, ask Cayley for help");
		}
	}
	
	public static Thing sqrtNeg(Thing x) throws FisherException {
		if(x.isNumber()) {
			double dx = x.asDouble(null);
			if (dx >= 0) {
				return new Complex(FloatTh.of(0.0), FloatTh.of(Math.sqrt(dx)));
			}
			else {
				return new Complex(FloatTh.of(Math.sqrt(-dx)), FloatTh.of(0.0));
			}
		} else {
			Doom.runtime("You tell me what language fiddle-dee-dee is, and I'll tell you French for it.", null);
		}
		return null;
		
	}
	
	
	public String toString() {
		try {
			return x() + "+" + y() + "i";
		} catch (FisherException e) {
			return "I can't print that, Dave";
		}
	}
	
	public double x() throws FisherException {return this.x.asDouble(null);}
	public double y() throws FisherException {return this.y.asDouble(null);}
	
	public Thing add(Thing z) throws FisherException {
		if (z instanceof Complex) {
			Complex cz = (Complex) z;
			double zx = cz.x.asDouble(null);
			double zy = cz.y.asDouble(null);
			return new Complex(FloatTh.of(x.asDouble(null) + zx), FloatTh.of(y.asDouble(null) + zy));
		}
		else {
			Doom.runtime("Yeah, it would be nice to be able to add reals to complexes.  Go write it, sir.", null);
			return null;
		}
	}
	
	@Override
	public boolean equals(Object obj) {
		try {
			if (obj instanceof Complex) {
				Complex z = (Complex) obj;
				double dx = z.x() - this.x();
				double dy = z.y() - this.y();
				return (dx*dx + dy*dy ) < 0.0000123; // I dunno about the numberical analysis really.
			} 
			else {
				return false;
			}
		} catch (FisherException e) {
			e.printStackTrace();
			return false;
		}
	}
}
