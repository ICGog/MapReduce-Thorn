package fisher.runtime.mapreduce;

import com.ericsson.otp.erlang.OtpSelf;
import com.ericsson.otp.erlang.*;

import fisher.runtime.Thing;
import fisher.runtime.IntTh;
import fisher.util.FisherException;

public class MapReduce { 
    static String copyright() {
        return fisher.util.Copyright.IBM_COPYRIGHT;
    }
    
    public static Thing scrieDracu(Thing a, Thing b) {
        try {
            OtpSelf self = new OtpSelf("thorn", "TQVNYQSBMPUYPPYFLTVR");
            OtpPeer other = new OtpPeer("erlide");
            OtpConnection connection = self.connect(other);
            
            OtpErlangObject[] msg = new OtpErlangObject[1];
			msg[0] = new OtpErlangInt(10);
			OtpErlangList l = new OtpErlangList(msg);
			connection.sendRPC("test","double",l);
			OtpErlangObject received = connection.receiveRPC();
			System.out.println(received.toString());
        }
        catch (Exception e) {
            return b;
        }
        return a;
    }
    
}
