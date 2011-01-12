package fisher.runtime.security;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;

/**
 * Wrapper class to create, hold, and check hashes
 * 
 * @author sguarni
 * @author mpistoia
 *
 */


public class Hash {
	private final byte[] hash;
	private static MessageDigest md;
	static {
		try {
			md = MessageDigest.getInstance("MD5");
		} catch (NoSuchAlgorithmException e) {
			md = null;
		}
	}

	public Hash(byte[] hash) {
		this.hash = hash;
	}
	
	public Hash(HashContents msg) {
		md.update(msg.getContents());
		hash = md.digest();
	}
	
	public boolean checkHash(HashContents msg) {
		md.update(msg.getContents());
		byte[] hashValue = md.digest();
		return Arrays.equals(hash, hashValue);
	}
	
	public byte[] getHashBytes() {
		return hash;
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + Arrays.hashCode(hash);
		return result;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Hash other = (Hash) obj;
		if (!Arrays.equals(hash, other.hash))
			return false;
		return true;
	}
	
	@Override
	public String toString() {
		return Arrays.toString(hash);
	}
	
//	public List<Integer> toIntList() {
//		List<Integer> list = new ArrayList<Integer>();
//		for (int i = 0; i < hash.length; i++) {
//			list.add((int) hash[i]);
//		}
//		return list;
//	}
}