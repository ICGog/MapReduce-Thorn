package fisher.runtime.security;

import java.io.Serializable;
import java.security.PublicKey;
import java.security.Signature;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import fisher.runtime.ListTh;
import fisher.runtime.Thing;
import fisher.util.Doom;
import fisher.util.FisherException;

/**
 * Main security class. Takes care of ensuring tokens are correctly validated and added to messages on send. Also takes care of 
 * reading tokens on receive and passing them back to thorn to give to the component. Might do other security things in the
 * future.
 * 
 * @author sguarni
 * @author mpistoia
 *
 */
public class Security {		
	private static Map<Thing, Signature> providers2Signatures = new HashMap<Thing, Signature>();
	
	public static Thing secure(Thing message, Thing securityInfo) throws FisherException {
		Thing sender = SecurityUtil.getCurrentComponentIDThing();
		if (securityInfo == null) {
			return ListTh.EMPTY;
		}
		//final Thing tokens = makeTokens(message, securityInfo);
		final Thing tokens = securityInfo;
		checkSecurity2(sender, message, tokens);
		return tokens;
	}
	
	public static void checkSecurity(Thing message, Thing securityThingList) throws FisherException {
		checkSecurity2(SecurityUtil.getCurrentComponentIDThing(), message, securityThingList);
	}

	private static void checkSecurity2(Thing receiver, Thing message, Thing securityThingList) throws FisherException {
		if (securityThingList instanceof ListTh) {
			ListTh securityListTh = (ListTh) securityThingList;
			List<SecurityToken> tokens;
			tokens = SecurityToken.loadSTFromListThing(securityListTh);
			checkSignedTokens(receiver, message, tokens);
		}
	}
	
	private static void checkSignedTokens(Thing comp, Thing msg, List<SecurityToken> tokens) throws FisherException {
		if (!areSignaturesValid(comp, tokens, msg)) {
			Doom.runtime("Security violation!", null, "signature was incorrect!");
		}
	}

	private static boolean areSignaturesValid(Thing comp, List<SecurityToken> l, Thing msg) throws FisherException {
		for (SecurityToken st : l) {
			if (!isSigValid(comp, msg, st)) {
				return false;
			}
		}
		return true;
	}	
	
	public static void addProvider(Thing provider, PublicKey pub) throws FisherException {
		try {
			if (!providers2Signatures.containsKey(provider)) {
				Signature verifierInstance = Signature.getInstance(SecurityProviderModule.SIG_ALGO);
				verifierInstance.initVerify(pub);
				providers2Signatures.put(provider, verifierInstance);
			}
		} catch (Exception e) {
			e.printStackTrace();
			Doom.runtime("No signature algorithm found", null, e);
		}
	}
	
	private static boolean isSigValid(Thing comp, Thing msg, SecurityToken st) {
		Thing k = st.getCapability();
		Thing c = st.getComponent();
		Thing spRef = st.getSPRef();
		byte[] sig = st.getSig();
		
		if (SecurityProviderModule.compToTrustedSPs.containsKey(comp)) {
			Set<Thing> s = SecurityProviderModule.compToTrustedSPs.get(comp);
			if (!s.contains(spRef)) {
				return false;
			}
		} else {
			return false;
		}
		
		try {
			Signature verifierInstance = providers2Signatures.get(spRef);
			if (verifierInstance == null) {
				return false;
			}
			verifierInstance.update(SecurityUtil.toByteArray(SecurityUtil.serializableToByteArray((Serializable) msg), k, c, spRef));
			return verifierInstance.verify(sig);
		} catch (Exception e) {
			e.printStackTrace();	
			return false;
		}
	}
}

