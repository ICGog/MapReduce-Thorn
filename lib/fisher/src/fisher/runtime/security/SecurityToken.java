package fisher.runtime.security;

import java.io.Serializable;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import fisher.eval.Evaller;
import fisher.runtime.BytesTh;
import fisher.runtime.ListTh;
import fisher.runtime.RecordTh;
import fisher.runtime.Thing;
import fisher.util.Doom;
import fisher.util.FisherException;

/**
 * Class that represents a security token. Contains the token itself and the Component that provided this token. The constructor
 * does the checking to make sure Component is allowed to add this token.
 * 
 * @author sguarni
 * @author mpistoia
 *
 */
public class SecurityToken implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 8266824713596257041L;
	private final Thing k;
	private byte[] sig;
	private final Thing c;
	private final Thing spRef;
	
	public SecurityToken(Thing k, Thing component, Thing spRef, byte[] sig) {
		this.k = k;
		this.c = component;
		this.spRef = spRef;
		this.sig = sig;
	}
	
	public Thing getCapability() {
		return k;
	}

	public byte[] getSig() {
		return sig;
	}

	public Thing getComponent() {
		return c;
	}

	public Thing getSPRef() {
		return spRef;
	}
	
	private static String[] thingFieldNames = {"k", "sig", "c", "spRef"};
	public RecordTh toThornObject() throws FisherException {
		Thing signedTokenAndMessageThing = new BytesTh(sig);
		return RecordTh.make(Evaller.lastSyntax(), thingFieldNames, k, signedTokenAndMessageThing, c, spRef);
	}
	
	
	@Override
	public String toString() {
		return "SecurityToken [k=" + k + ", sig=" + Arrays.toString(sig)
				+ ", c=" + c + ", spRef=" + spRef + "]";
	}

	/*************************************************************/
	/*************************************************************/
	/* Static methods */
	/*************************************************************/
	/*************************************************************/
	
	//private static String[] thingFieldNames = {"k", "sig", "c", "spRef"};
	public static SecurityToken loadSTFromThing(Thing stTh) throws FisherException {
		if (!(stTh.isRecord())) {
			Doom.runtime("stTh was not a record", null, stTh);
		}
		RecordTh recTh = (RecordTh) stTh;
		if (!recTh.hasField("k") || !recTh.hasField("sig") || !recTh.hasField("c") || !recTh.hasField("spRef")) {
			Doom.runtime("fields were not in the security object", null, recTh);
			return null;
		}
		Thing capTh = recTh.getField("k", Evaller.lastSyntax());
		Thing compThing = recTh.getField("c", Evaller.lastSyntax());
		Thing sigThing = recTh.getField("sig", Evaller.lastSyntax());
		Thing spRefThing = recTh.getField("spRef", Evaller.lastSyntax());
		byte[] sigBytes;
		if (!sigThing.isBytes()) {
			Doom.runtime("sig was not BytesTh", null, sigThing);
			return null;
		}
		sigBytes = sigThing.asBytes(Evaller.lastSyntax());
		return new SecurityToken(capTh, compThing, spRefThing, sigBytes);
	}
	
	public static List<SecurityToken> loadSTFromListThing(ListTh l) throws FisherException {
		List<SecurityToken> ret = new LinkedList<SecurityToken>();
		for (Thing t : l) {
			SecurityToken st = loadSTFromThing(t);
			ret.add(st);
		}
		return ret;
	}
	
	public static ListTh toThornList(List<SecurityToken> tokens) throws FisherException {
		List<Thing> thingList = new LinkedList<Thing>();
		for (SecurityToken st : tokens) {
			thingList.add(st.toThornObject());
		}
		return ListTh.fromJavaCollection(thingList);
	}
}
