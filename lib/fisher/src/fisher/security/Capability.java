
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

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.Arrays;

import fisher.runtime.BoolTh;
import fisher.runtime.Thing;
import fisher.runtime.ThingExtended;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  Capability extends ThingExtended  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public static final int KEY_LEN = 128;
	
	private static SecureRandom random = null;
	
	private byte[] privateId, publicId;
	
	public byte[] getPublicId() {
		return publicId;
	}

	static byte[] createId() throws NoSuchAlgorithmException {
		if (random == null) {
			synchronized (Capability.class) {
				if (random == null) {
					random = SecureRandom.getInstance("SHA1PRNG");
				}
			}
		}
		
		byte[] ret = new byte[KEY_LEN];
		random.nextBytes(ret);
		return ret;
	}
	
	public Capability() throws FisherException {
		try {
			privateId = createId();
			publicId = createId();
		} catch (NoSuchAlgorithmException e) {
			Doom.runtime("Failed to create capability.", null);
		}
	}
	
	public byte[] createKey(byte[] pubId, byte[] code) throws NoSuchAlgorithmException {
		assert(pubId.length == KEY_LEN && code.length == KEY_LEN);
		
		byte[] combine, hash;
		
		// create the combined code
		combine = new byte[KEY_LEN * 3];
		System.arraycopy(privateId, 0, combine, 0, KEY_LEN);
		System.arraycopy(pubId, 0, combine, KEY_LEN, KEY_LEN);
		System.arraycopy(code, 0, combine, KEY_LEN*2, KEY_LEN);
		
		// get the hash
		MessageDigest sha = MessageDigest.getInstance("SHA-256");
		hash = sha.digest(combine);
		
		return hash;
	}
	
	public boolean verify(CapabilityKey ckey) throws FisherException {
		byte[] realKey;
		if (!Arrays.equals(ckey.getCapId(), publicId)) return false;
		
		try {
			realKey = createKey(ckey.getCapId(), ckey.getCode());
		} catch (NoSuchAlgorithmException e) {
			Doom.runtime("Failed to create a capability key.", null, this);
			return false;
		}
		byte[] gotKey = ckey.getKey();
		if (Arrays.equals(realKey, gotKey)) return true;
		return false;
	}
	
	public Thing verifyTh(Thing key) throws FisherException {
		if (key instanceof CapabilityKey) {
			return verify((CapabilityKey) key) ? BoolTh.True : BoolTh.False;
		}
		return BoolTh.False;
	}
}
