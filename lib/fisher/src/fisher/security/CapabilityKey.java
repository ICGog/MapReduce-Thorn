
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.security;

import java.security.NoSuchAlgorithmException;

import fisher.runtime.Thing;
import fisher.runtime.ThingExtended;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  CapabilityKey extends ThingExtended  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	private byte[] capId, code, key;

	public byte[] getCapId() {
		return capId;
	}

	public void setCode(byte[] code) {
		this.code = code;
	}

	public byte[] getCode() {
		return code;
	}

	public byte[] getKey() {
		return key;
	}
	
	public CapabilityKey(Thing cap) throws FisherException {
		if (cap instanceof Capability) {
			Capability ccap = (Capability) cap;
			try {
				capId = ccap.getPublicId();
				code = Capability.createId();
				key = ccap.createKey(capId, code);
			} catch (NoSuchAlgorithmException e) {
				Doom.runtime("Failed to create a capability key.", null, cap);
			}
		} else {
			Doom.runtime("Trying to create a capability key from a non-capability.", null, cap);
		}
	}
}
