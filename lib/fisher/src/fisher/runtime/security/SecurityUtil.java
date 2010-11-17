package fisher.runtime.security;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

import fisher.runtime.Thing;
import fisher.runtime.dist.ComponentThread;
import fisher.util.Doom;
import fisher.util.FisherException;

public class SecurityUtil {

	public static byte[] serializableToByteArray(Serializable t) throws IOException {
		ByteArrayOutputStream bos = new ByteArrayOutputStream() ;
		ObjectOutputStream out;
		out = new ObjectOutputStream(bos);
		out.writeObject(t);
		return bos.toByteArray();
	}
	
	public static Object unserializeByteArray(byte[] b) throws IOException, ClassNotFoundException {
		ByteArrayInputStream bis = new ByteArrayInputStream(b);
		ObjectInputStream ois = new ObjectInputStream(bis);
		
		return ois.readObject();
	}
	
	private static int fillData(byte[] src, byte[] dest, int startPos) {
		for (int i = 0; i < src.length; ++i) {
			dest[startPos++] = src[i];
		}
		return startPos;
	}
	
	public static byte[] toByteArray(byte[] msg, Thing k, Thing c, Thing spRef) throws FisherException {
		byte[] capAsBytesArray;
		byte[] spRefAsBytesArray;
		byte[] compAsByteArray;
		try {
			capAsBytesArray = SecurityUtil.serializableToByteArray((Serializable) k);
			spRefAsBytesArray = SecurityUtil.serializableToByteArray((Serializable) spRef);
			compAsByteArray = SecurityUtil.serializableToByteArray((Serializable) c);
		} catch (IOException e) {
			e.printStackTrace();
			Doom.runtime("Couldn't serialize the Thing", null, k, spRef);
			capAsBytesArray = null;
			spRefAsBytesArray = null;
			compAsByteArray = null;
		}
		int size = msg.length + capAsBytesArray.length + compAsByteArray.length + spRefAsBytesArray.length;
		byte[] data = new byte[size];
		int pos = 0;
		pos = fillData(msg, data, pos);
		pos = fillData(capAsBytesArray, data, pos);
		pos = fillData(compAsByteArray, data, pos);
		pos = fillData(spRefAsBytesArray, data, pos);
		
		return data;
	}
	
	public static Thing getCurrentComponentIDThing() {
		return ComponentThread.mine().compTh;
	}
}
