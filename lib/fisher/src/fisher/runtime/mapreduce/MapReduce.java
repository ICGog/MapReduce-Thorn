package fisher.runtime.mapreduce;

import com.ericsson.otp.erlang.OtpSelf;
import com.ericsson.otp.erlang.*;

import fisher.runtime.Thing;
import fisher.runtime.StringTh;
import fisher.runtime.ListTh;
import fisher.runtime.IntTh;
import fisher.runtime.BytesTh;
import fisher.util.FisherException;
import java.util.*;

/**
 * This is experimental code. Please implement this properly.
 * The authCookie should not be hardcoded, but it should somehowe be passed
 * to the Thorn interpreter, so that we don't have to recompile fisher
 * whenever we want to change the cookie.
 */

public class MapReduce {

    private static final int MAX_BUFFER_SIZE = 64 * 1024 * 1024;

    private static Map<String, Buffer> bin = new HashMap<String, Buffer>();
    private static Map<String, Buffer> bout = new HashMap<String, Buffer>();

    private static int bufferBytes = 50;
    private static OtpConnection erlangConnection = null;

    static String copyright() {
        return fisher.util.Copyright.IBM_COPYRIGHT;
    }

    public static Pair thingToPair(Thing thPair) {
        return new Pair((byte[]) ((ListTh) thPair).car().unthingify(), 
            (byte[]) ((ListTh) thPair).cdr().car().unthingify());
    }

    public static Thing pairToThing(Pair p) {
        try {
            return ListTh.of(new BytesTh(p.key), new BytesTh(p.value));
        } catch (Exception e) {
            return StringTh.of("error");
        }
    }

    public static OtpErlangTuple pairToErlang(Pair p) {
        OtpErlangObject[] tuple = new OtpErlangObject[2];
        tuple[0] = new OtpErlangBinary(p.key);
        tuple[1] = new OtpErlangBinary(p.value);
        return new OtpErlangTuple(tuple);
    }

    public static Pair erlangToPair(OtpErlangObject erlTuple) {
            return new Pair(((OtpErlangBinary) ((OtpErlangTuple) erlTuple)
                .elementAt(0)).binaryValue(),
            ((OtpErlangBinary) ((OtpErlangTuple) erlTuple)
                .elementAt(1)).binaryValue());
    }
    
    public static Pair2 erlangToPair2(OtpErlangObject erlTuple) {
        byte[] key = ((OtpErlangBinary) ((OtpErlangTuple) erlTuple).elementAt(0)).binaryValue();
        OtpErlangList valuesList = (OtpErlangList) ((OtpErlangTuple) erlTuple).elementAt(1);
        byte[][] values = new byte[valuesList.arity()][];
        int i = 0;
        for(OtpErlangObject erlValue : (OtpErlangList) ((OtpErlangTuple) erlTuple)
                .elementAt(1)) {
            values[i++] = ((OtpErlangBinary) erlValue).binaryValue();
        }
        
        return new Pair2(key, values);
    }
    
    public static ListTh pair2ToThing(Pair2 p) {
        try {
            ListTh values = ListTh.of();
            for(int i = 0; i < p.values.length; i++)
                values = values.cons(new BytesTh(p.values[i]));
            return ListTh.of(new BytesTh(p.key), values);
        } catch (Exception e) {
            e.printStackTrace();
            System.out.println("Error pair2ToThing");
        }
        return null;
    }

    public static Thing newJob(Thing m, Thing r, Thing cookie, Thing peer){
       try {
            Buffer b = new Buffer();

            // Setup the communication with the erlang node
            OtpSelf self = new OtpSelf("thorn", cookie.toString());
            // The name of the node with which we're communicating.
            OtpPeer other = new OtpPeer(peer.toString());
            if (erlangConnection == null)
                erlangConnection = self.connect(other);

            // Create the list of args of the erlang function we're calling
            OtpErlangObject[] workerLangTuple = new OtpErlangObject[2];
            workerLangTuple[0] = new OtpErlangAtom("worker_lang");
            workerLangTuple[1] = new OtpErlangAtom("thorn");
            OtpErlangObject[] argErlObj = new OtpErlangObject[3];
            argErlObj[0] = new OtpErlangString(m.toString()); // Add the map function
            argErlObj[1] = new OtpErlangString(r.toString()); // Add the reduce function
            argErlObj[2] = new OtpErlangList(new OtpErlangTuple(workerLangTuple));
            OtpErlangList args = new OtpErlangList(argErlObj);

            // Change the names of the function to match the erlang one
            erlangConnection.sendRPC("smr_master", "new_job", args);
            OtpErlangObject received = erlangConnection.receiveRPC();
            // Extract the jobId and then send it back to thorn
            String code = ((OtpErlangTuple) received).elementAt(0).toString();
            if (!code.equals("ok")) {
                    return StringTh.of("Error");
            }

            String jobId = ((OtpErlangTuple) received).elementAt(1).toString();
            bin.put(jobId, b);
            
            return StringTh.of(jobId);
       } catch (Exception e) {
            return StringTh.of("Error");
       }
    }

    public static void emit(Thing thJobId, Thing kv){
       try{
           // kv is a list like [key, value]
           // String jbid = jobId.toString();
           String jobId = thJobId.toString();
           Buffer b = bin.get(jobId);
           Pair p = thingToPair(kv);
           b.buffer.add(p);
           b.byteSize += p.size();
           
           if (b.byteSize >= MAX_BUFFER_SIZE) {
               sendKVPairsToErlang(jobId);
           }
       } catch (Exception e) {
           e.printStackTrace();
       }    
    }

    private static void sendKVPairsToErlang(String jobId){
       try{
           Buffer b = bin.remove(jobId);
           bin.put(jobId, new Buffer());
           
           // Send the buffer to Erlang
           OtpErlangObject[] dataObjList = new OtpErlangObject[b.buffer.size()];
           int i = 0;
           for (Pair p : b.buffer)
               dataObjList[i++] = pairToErlang(p);
           OtpErlangList dataList = new OtpErlangList(dataObjList);

           // Send remainder of data to Erlang
           OtpErlangObject[] remErlObj = new OtpErlangObject[2];
           remErlObj[0] = new OtpErlangLong(Long.parseLong(jobId));
           remErlObj[1] = dataList;
           OtpErlangList args = new OtpErlangList(remErlObj);

           // Change the names of the function to match the erlang one
           erlangConnection.sendRPC("smr_master", "add_input", args);
           erlangConnection.receiveRPC();
       } catch (Exception e) {
           e.printStackTrace();
       }
    }
    
    public static Thing doJob(Thing thJobId){
        try{
            String jobId = thJobId.toString();

            sendKVPairsToErlang(jobId);
            
            // Create the list of args of the erlang function we're calling
            OtpErlangObject[] argErlObj = new OtpErlangObject[1];
            argErlObj[0] = new OtpErlangLong(Long.parseLong(jobId));
            OtpErlangList args = new OtpErlangList(argErlObj);

            bout.put(jobId, new Buffer());

            erlangConnection.sendRPC("smr_master", "do_job", args);
            OtpErlangObject received = erlangConnection.receiveRPC();
            return StringTh.of(received.toString());
        
        } catch (Exception e) {
            return StringTh.of("error");
       }
    }
    
    public static void killJob(Thing thJobId){
        try{
        // Create the list of args of the erlang function we're calling
        String jobId = thJobId.toString();
        bin.remove(jobId);
        bout.remove(jobId);
        OtpErlangObject[] argErlObj = new OtpErlangObject[1];
        argErlObj[0] = new OtpErlangLong(Long.parseLong(jobId));
        OtpErlangList args = new OtpErlangList(argErlObj);

        erlangConnection.sendRPC("smr_master", "kill_job", args);
        erlangConnection.receiveRPC();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    private static void getNextResult(String jobId){
        try{
            Buffer b = bout.get(jobId);
            
            OtpErlangObject[] msg = new OtpErlangObject[1];
            msg[0] = new OtpErlangLong(Long.parseLong(jobId));
            OtpErlangList l = new OtpErlangList(msg);

            // The Name of the module we're communicating with.
            erlangConnection.sendRPC("smr_master", "next_result", l);
            OtpErlangObject received = erlangConnection.receiveRPC();
            if (received.toString().equals("end_of_result"))
                return;

            // Assume that I get back from erlang somethign like this: [ { k1, v1 }, ... ]
            for(OtpErlangObject obj : (OtpErlangList) received){
                b.buffer.add(erlangToPair(obj));
            }
       } catch(Exception e) {
          System.out.println("Error");
          e.printStackTrace();
       }
    }

    public static Thing retrieve(Thing thJobId){
       try{
           String jobId = thJobId.toString();
           Buffer b = bout.get(jobId);
           if (b.buffer.size() == 0)
           {
               getNextResult(jobId);
               if (b.buffer.size() == 0)
               {
                   bout.remove(jobId);
                   return StringTh.of("end_of_result");
               }
           }

           return pairToThing(b.buffer.remove(0));
       } catch (Exception e) {
           return StringTh.of("error");
       }    
    }

}

