package fisher.runtime.security;

import java.util.Arrays;

/**
 * Container class to hold what will be put into a message. Main purpose is to make it easy to change 
 * what goes into a message by only changing how this class is constructed. Uses of getContents should
 * never have to change.
 * 
 * @author sguarni
 * @author mpistoia
 *
 */
public class HashContents {
	public final byte[] message;

	public HashContents(byte[] msg) {
		this.message = msg;
	}
	
	public byte[] getContents() {
		return message;
	}

	@Override
	public String toString() {
		return "HashContents [message=" + Arrays.toString(message) + "]";
	}
	
	
}