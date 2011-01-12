package fisher.runtime.security;

import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import fisher.runtime.BytesTh;
import fisher.runtime.Thing;
import fisher.util.Doom;
import fisher.util.FisherException;

public class ComponentsToCapabilities {
	private static final String RANDOM_NUMBER_GENERATOR = "SHA1PRNG";
	private static final int RANDOM_CAPABILITY_SIZE = 48;
	
	private Map<Thing, Set<Thing>> map;
	private Set<Thing> capabilities;
	private SecureRandom random;
	
	private Set<Thing> findOrCreateSetOfCapsForComponent(Thing key) {
		if (map.containsKey(key)) {
			return map.get(key);
		} else {
			Set <Thing> s = new HashSet<Thing>();
			map.put(key, s);
			return s;
		}
	}
	
	public ComponentsToCapabilities() throws FisherException {
		map = new HashMap<Thing, Set<Thing>>();
		capabilities = new HashSet<Thing>();
		try {
			random = SecureRandom.getInstance(RANDOM_NUMBER_GENERATOR);
		} catch (NoSuchAlgorithmException e) {
			e.printStackTrace();
			Doom.runtime("PRNG algorithm not found", null, RANDOM_NUMBER_GENERATOR);
		}
	}

	public Thing createNewRandomCapability(Thing granter) {
		byte[] bytes = new byte[RANDOM_CAPABILITY_SIZE];
		random.nextBytes(bytes);
		BytesTh cap = new BytesTh(bytes);
		//this really should always be false but there is a chance that it won't be. For instance if someone inserted something
		//as a capability and it happens to match exactly the random number produced by the PRNG.
		assert(!capabilities.contains(cap));
		capabilities.add(cap);
		findOrCreateSetOfCapsForComponent(granter).add(cap);
		return cap;
	}
	
	public boolean grantCapability(Thing granter, Thing k, Thing c) {
		Set<Thing> granterCaps = findOrCreateSetOfCapsForComponent(granter);
		Set<Thing> receiverCaps = findOrCreateSetOfCapsForComponent(c);
		if (capabilities.contains(k)) {
			if (!granterCaps.contains(k)) {
				return false;
			}
		} else {
			capabilities.add(k);
		}
		receiverCaps.add(k);
		return true;
	}
	
	public boolean checkComponentHasCapability(Thing k, Thing c) {
		if (map.containsKey(c)) {
			Set<Thing> s = map.get(c);
			return s.contains(k);
		}
		return false;
	}
}