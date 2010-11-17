package fisher.runtime.security;

/**
 * Simple exception for when a component is not allowed to add a specified token to a message
 * 
 * @author sguarni
 * @author mpistoia
 *
 */
public class InvalidTokenException extends Exception {

	public InvalidTokenException(String msg) {
		super(msg);
	}

	/**
	 * 
	 */
	private static final long serialVersionUID = 2318426380220326391L;

}
