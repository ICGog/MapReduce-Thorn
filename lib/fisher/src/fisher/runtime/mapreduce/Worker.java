package fisher.runtime.mapreduce;

import com.ericsson.otp.erlang.OtpSelf;
import com.ericsson.otp.erlang.*;

import fisher.runtime.Thing;
import fisher.runtime.ListTh;
import fisher.runtime.BoolTh;
import fisher.util.FisherException;
import java.util.*;

public class Worker {

    private static OtpConnection erlangConnection = null;

    private static void createConnection(Thing peer, Thing cookie)
            throws Exception {
        OtpSelf self = new OtpSelf("thorn_task", cookie.toString());
        OtpPeer worker = new OtpPeer(peer.toString());
        if (erlangConnection == null) {
            erlangConnection = self.connect(worker);
        }
    }

    public static ListTh getInput(Thing type, Thing peer, Thing cookie) {
        try {
            createConnection(peer, cookie);
            
            ListTh listTh = ListTh.of();
            OtpErlangList args = new OtpErlangList();
            erlangConnection.sendRPC("smr_worker", "get_input", args);

            OtpErlangTuple received = (OtpErlangTuple) erlangConnection.receiveRPC();

            if (!received.elementAt(0).toString().equals("ok")) {
                // TODO: Error
                System.out.println("error 8");
            }
            OtpErlangObject[] input = ((OtpErlangList)(received.elementAt(1))).elements();
            for (int i = 0; i < input.length; ++i) {
                if (type.toString().equals("map")) {
                    listTh = listTh.cons(MapReduce.pairToThing(MapReduce.erlangToPair(input[i])));
                } else {
                    listTh = listTh.cons(MapReduce.pair2ToThing(MapReduce.erlangToPair2(input[i])));
                }
            }
            return listTh;
        } catch (Exception e) {
            System.out.println("error 7");
            e.printStackTrace();
        }
        return null;
    }

    public static void sendOutput(Thing output) {
        try {
            List<Thing> list = ((ListTh)output).toJavaList();
            OtpErlangTuple[] elem = new OtpErlangTuple[list.size()];
            int i = 0;
            for (Thing thing : list) {
                elem[i++] = MapReduce.pairToErlang(MapReduce.thingToPair(thing));
            }
            OtpErlangList args = new OtpErlangList(new OtpErlangList(elem));
            erlangConnection.sendRPC("smr_worker", "send_output", args);
            OtpErlangAtom received = (OtpErlangAtom) erlangConnection.receiveRPC();

            if (!received.toString().equals("ok")) {
                // TODO: Error
                System.out.println("Error 1");
            }
        } catch (Exception e) {
            // TODO: Error
            System.out.println("Error 2");
        }
    }

}

