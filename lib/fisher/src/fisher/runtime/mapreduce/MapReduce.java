package fisher.runtime.mapreduce;

import com.ericsson.otp.erlang.OtpSelf;
import com.ericsson.otp.erlang.*;

import fisher.runtime.Thing;
import fisher.runtime.IntTh;
import fisher.util.FisherException;

/**
 * This is experimental code. Please implement this properly.
 * The authCookie should not be hardcoded, but it should somehowe be passed
 * to the Thorn interpreter, so that we don't have to recompile fisher
 * whenever we want to change the cookie.
 */

public class MapReduce { 

    static String copyright() {
        return fisher.util.Copyright.IBM_COPYRIGHT;
    }
    
    public static Thing doubleNum(Thing a) {
        try {
            OtpSelf self = new OtpSelf("thorn", "authCookie");
            // The name of the node with which we're communicating.
            // TODO: Again if possible this should not be hardcoded.
            OtpPeer other = new OtpPeer("erlide");
            OtpConnection connection = self.connect(other);
            
            OtpErlangObject[] msg = new OtpErlangObject[1];
			msg[0] = new OtpErlangInt(10);
			OtpErlangList l = new OtpErlangList(msg);

            // The Name of the module we're communicating with.
			connection.sendRPC("integration", "double", l);
			OtpErlangObject received = connection.receiveRPC();
			System.out.println(received);
        }
        catch (Exception e) {
            // TODO: Catch the right exceptions and handle them accordingly.
        }
        return a;
    }
    
}
