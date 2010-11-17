
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.runtime.builtInFun;

import java.net.URI;
import java.net.URISyntaxException;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.BuiltInFunctionTh;
import fisher.runtime.SiteTh;
import fisher.runtime.Thing;
import fisher.runtime.dist.ComponentThread;
import fisher.syn.core.Syntax;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  SiteBIF extends BuiltInFunctionTh  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	@Override
	public Thing apply(Thing[] args, Framelike ignoredFrame, Evaller evaller, Syntax src) throws FisherException {
		checkNumberOfArgs(1, 2, "()", args, evaller, ignoredFrame, src);
		if (args.length == 2) {
			String ip = EvalUtil.toString(args[0]);
			int port = (int) (EvalUtil.asLong(args[1], src));
			return new SiteTh(ip, port);
		} else if (args.length == 1) {
			String uris = EvalUtil.toString(args[0]);
			try {
				URI uri = new URI(uris);
				String ip = uri.getHost();
				int port = uri.getPort();
				return new SiteTh(ip, port);
			} catch (URISyntaxException e) {
				Doom.runtime("URI syntax error: " + e.toString() + "\nuri=" + uris + "\n", src, uris);
				return null;
			}

		} else {
			Doom.runtime("Math is broken!", src);
			return null;
		}

	}

}
