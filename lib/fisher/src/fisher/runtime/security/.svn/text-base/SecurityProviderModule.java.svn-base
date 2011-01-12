package fisher.runtime.security;

import java.io.Serializable;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.Signature;
import java.security.SignatureException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import fisher.runtime.Thing;
import fisher.util.Doom;
import fisher.util.FisherException;

public class SecurityProviderModule {
	private static final String ENCRYPTION_ALGO = "DSA"; 
	private static final String HASH_ALGO = "SHA1";
	private static final int KEYSIZE = 1024;
	public static final String SIG_ALGO = HASH_ALGO + "with" + ENCRYPTION_ALGO;
	
	private static final Map<Thing, Signature> spRefToSigners = new HashMap<Thing, Signature>();
	private static final Map<Thing, ComponentsToCapabilities> spToComponentToCapabilities = new HashMap<Thing, ComponentsToCapabilities>();
	
	private static ComponentsToCapabilities findOrCreateComponentsToCapabilities(Thing spRef) throws FisherException {
		ComponentsToCapabilities componentsToCapabilities;
		if (spToComponentToCapabilities.containsKey(spRef)) {
			componentsToCapabilities = spToComponentToCapabilities.get(spRef);
		} else {
			componentsToCapabilities = new ComponentsToCapabilities();
			spToComponentToCapabilities.put(spRef, componentsToCapabilities);
		}
		return componentsToCapabilities;
	}
	
	private static byte[] sign(Thing k, byte[] message, Thing c, Thing spRef) throws FisherException {
		Signature signerInstance = spRefToSigners.get(spRef);
		if (signerInstance == null) {
			Doom.runtime("Not a valid signer", null, spRef);
			return null;
		}
		try {
			signerInstance.update(SecurityUtil.toByteArray(message, k, c, spRef));
			return signerInstance.sign();
		} catch (SignatureException e) {
			e.printStackTrace();
			Doom.runtime("Signature exception", null, e);
			return null;
		}
	}
	
	public static Thing generateNewCapability(Thing granter) throws FisherException {
		Thing spRef = SecurityUtil.getCurrentComponentIDThing();
		ComponentsToCapabilities componentsToCapabilities = findOrCreateComponentsToCapabilities(spRef);
		return componentsToCapabilities.createNewRandomCapability(granter);
	}
	
	public static Thing generateAndGrantNewCapability(Thing granter, Thing c) throws FisherException {
		Thing spRef = SecurityUtil.getCurrentComponentIDThing();
		ComponentsToCapabilities componentsToCapabilities = findOrCreateComponentsToCapabilities(spRef);
		Thing k = componentsToCapabilities.createNewRandomCapability(granter);
		componentsToCapabilities.grantCapability(granter, k, c);
		return k;
	}		
	
	public static Thing grantCapability(Thing granter, Thing k, Thing c) throws FisherException {
		Thing spRef = SecurityUtil.getCurrentComponentIDThing();
		ComponentsToCapabilities componentsToCapabilities = findOrCreateComponentsToCapabilities(spRef);
		if (componentsToCapabilities.grantCapability(granter, k, c)) {
			return k;
		} else {
			return null;
		}
	}

	public static Thing makeToken(Thing msg, Thing k, Thing c) throws FisherException {
		Thing spRef = SecurityUtil.getCurrentComponentIDThing();
		if (!spToComponentToCapabilities.containsKey(spRef)) {
			Doom.runtime("This SP doesn't have any capabilities", null, spRef, c, k);
		}
		ComponentsToCapabilities componentsToCapabilities = spToComponentToCapabilities.get(spRef);
		if (!componentsToCapabilities.checkComponentHasCapability(k, c)) {
			Doom.runtime("This component doesn't have this capability", null, spRef, c, k);
		}
		try {
			byte[] message = SecurityUtil.serializableToByteArray((Serializable) msg);
			byte[] sig = sign(k, message, c, spRef);
			return new SecurityToken(k, c, spRef, sig).toThornObject();
		} catch (Exception e) {
			e.printStackTrace();
			Doom.runtime("Could not create security token", null, msg, k, c, spRef);
			return null;
		}
	}

	public static Thing initSecurityProvider() throws FisherException {
		Thing spRef = SecurityUtil.getCurrentComponentIDThing();
		try {
			Signature signerInstance;
			KeyPairGenerator kpg = KeyPairGenerator.getInstance(ENCRYPTION_ALGO);
			SecureRandom r = new SecureRandom();
			kpg.initialize(KEYSIZE, r);
			KeyPair kp = kpg.generateKeyPair();
			PrivateKey priv = kp.getPrivate();
			PublicKey pub = kp.getPublic();
			Security.addProvider(spRef, pub);
			signerInstance = Signature.getInstance(SIG_ALGO);
			signerInstance.initSign(priv);
			SecurityProviderModule.spRefToSigners.put(spRef, signerInstance);
		} catch (Exception e) {
			e.printStackTrace();
			Doom.runtime("No signature algorithm found", null, e);
		}
		return null;
	}
	
	public static Map<Thing, Set<Thing>> compToTrustedSPs = new HashMap<Thing, Set<Thing>>();
	public static Thing trust(Thing spRef) {
		Thing c = SecurityUtil.getCurrentComponentIDThing();
		Set<Thing> s;
		if (compToTrustedSPs.containsKey(c)) {
			s = compToTrustedSPs.get(c);
		} else {
			s = new HashSet<Thing>();
			compToTrustedSPs.put(c, s);
		}
		s.add(spRef);
		return null;
	}
}
