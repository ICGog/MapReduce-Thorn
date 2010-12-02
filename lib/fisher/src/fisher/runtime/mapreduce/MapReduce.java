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

    	static String copyright() {
        	return fisher.util.Copyright.IBM_COPYRIGHT;
    	}

	public static Thing doubleNum(Thing a) {
		return a;
	}

	public static Thing sendBytesToErlang(Thing m, Thing r, Thing kvp, Thing cookie, Thing peer){
	  try {
		//Setup the communication with the erlang node
		OtpSelf self = new OtpSelf("thorn", cookie.toString());
            	// The name of the node with which we're communicating.
            	OtpPeer other = new OtpPeer(peer.toString());
            	OtpConnection connection = self.connect(other);
		
		//create the list of args of the erlang function we're calling
		OtpErlangObject[] argErlObj = new OtpErlangObject[3];
		argErlObj[0] = new OtpErlangString(m.toString()); //add the map function
		argErlObj[1] = new OtpErlangString(r.toString()); //add the reduce function

		//create the list of key-value pairs from the list passed as argument from Thorn
		ListTh kvpList = (ListTh) kvp;
		int len = kvpList.length; 
		OtpErlangObject[] kvpErlObj = new OtpErlangObject[len];
		int j = 0;
		byte[] key;
		byte[] val;
            	for (Thing elemThing : kvpList.toJavaArray()) {
                	ListTh elem = (ListTh) elemThing;
                	key = (byte[]) elem.car().unthingify();
			val = (byte[]) elem.cdr().car().unthingify();
			OtpErlangObject[] tuple = new OtpErlangObject[2];
			tuple[0] = new OtpErlangBinary(key);
			tuple[1] = new OtpErlangBinary(val);
			OtpErlangTuple t = new OtpErlangTuple(tuple);
			kvpErlObj[j] = t;
			j++;
            	}
		OtpErlangList argErlList = new OtpErlangList(kvpErlObj);

		argErlObj[2] = argErlList; //add the list of kvp pairs
		OtpErlangList args = new OtpErlangList(argErlObj);

		// The Name of the module we're communicating with.
		// The list of args is of the form: [ map, reduce, [ (key, value), ... ] ]
	   	connection.sendRPC("integration", "getBytesFromThorn", args); //change the names of the function to match the erlang one
	    	OtpErlangObject received = connection.receiveRPC();
		System.out.println(received);
		connection.close();
	  } catch (Exception e) {
	  }
	  return m;
	}

	public static Thing getBytesFromErlang(Thing list, Thing cookie, Thing peer){
	   OtpErlangObject received = null;
	   ListTh res = (ListTh) list;
	   try {
		//Setup the communication with the erlang node
		OtpSelf self = new OtpSelf("thorn", cookie.toString());
            	// The name of the node with which we're communicating.
            	OtpPeer other = new OtpPeer(peer.toString());
            	OtpConnection connection = self.connect(other);

		OtpErlangObject[] msg = new OtpErlangObject[1];
		msg[0] = new OtpErlangString("getBytes");
		OtpErlangList l = new OtpErlangList(msg);

            	// The Name of the module we're communicating with.
	    	connection.sendRPC("integration", "sendBytesToThorn", l);
	    	received = connection.receiveRPC();

		//assume that I get back from erlang somethign like this: [ ( bv1, bv2 ), ... ]

		OtpErlangList liist = (OtpErlangList) received;
                int lenn = liist.elements().length;
		BytesTh key;
		BytesTh val;
                for(int i=0; i<lenn; i++){
                        key = new BytesTh(((OtpErlangBinary)((OtpErlangTuple)liist.elementAt(i)).elementAt(0)).binaryValue());//.toString().getBytes()); //not sure about this
                        val = new BytesTh(((OtpErlangBinary)((OtpErlangTuple)liist.elementAt(i)).elementAt(1)).binaryValue());//.toString().getBytes()); //not sure about this
                        //add the key val to the list
			res.cons(key);
			res.cons(val);
                        //the list should now be of the form [ val, key, val, key, ... ]
                }
		//reverse the list to get elements in the right order
		//in thorn we must then transform the list to [[key, val], ... ]
		res = res.reversed();
		connection.close();
	   } catch(Exception e) {
	 	
	   }
	   return res;
	}

	public static Thing sendStringToErlang(Thing m, Thing r, Thing kvp, Thing length, Thing cookie, Thing peer){
	   try {
		//Setup the communication with the erlang node
		OtpSelf self = new OtpSelf("thorn", cookie.toString());
            	// The name of the node with which we're communicating.
            	OtpPeer other = new OtpPeer(peer.toString());
            	OtpConnection connection = self.connect(other);

		//create the list of args of the erlang function we're calling
		OtpErlangObject[] argErlObj = new OtpErlangObject[3];
		argErlObj[0] = new OtpErlangString(m.toString()); //add the map function
		argErlObj[1] = new OtpErlangString(r.toString()); //add the reduce function

		//assume we have a list of 2-lists given as a string
		//[ [k,v], ... ]
		String list = kvp.toString();
		//I need the length of the list of values
		int len = Integer.parseInt(length.toString());
		StringTokenizer st = new StringTokenizer(list, " ,[]\n\t");
		OtpErlangObject[] kvpErlObj = new OtpErlangObject[len];
		int j = 0;
		String k;
		String v;
		while(st.hasMoreTokens()){
			k = st.nextToken();
			v = st.nextToken();
			OtpErlangObject[] tuple = new OtpErlangObject[2];
			tuple[0] = new OtpErlangString(k);
			tuple[1] = new OtpErlangString(v);
			OtpErlangTuple t = new OtpErlangTuple(tuple);
			kvpErlObj[j] = t;
			j++;
		}
		OtpErlangList argErlList = new OtpErlangList(kvpErlObj);

		argErlObj[2] = argErlList; //add the list of kvp pairs
		OtpErlangList args = new OtpErlangList(argErlObj);

		// The Name of the module we're communicating with.
	   	connection.sendRPC("integration", "getStringsFromThorn", args); //change the names of the function to match the erlang one
	    	OtpErlangObject received = connection.receiveRPC();
		System.out.println(received);
		connection.close();
	  } catch (Exception e) {
	  }
	  return m;
	}

	public static Thing getStringFromErlang(Thing cookie, Thing peer){
           OtpErlangObject received = null;
	   try {
		//Setup the communication with the erlang node
		OtpSelf self = new OtpSelf("thorn", cookie.toString());
            	// The name of the node with which we're communicating.
            	OtpPeer other = new OtpPeer(peer.toString());
            	OtpConnection connection = self.connect(other);

		OtpErlangObject[] msg = new OtpErlangObject[1];
		msg[0] = new OtpErlangString("getString");
		OtpErlangList l = new OtpErlangList(msg);

            	// The Name of the module we're communicating with.
	    	connection.sendRPC("integration", "sendStringsToThorn", l);
	    	received = connection.receiveRPC();
	    	//System.out.println(received);
		connection.close();
	   } catch(Exception e) {
	 	System.out.println(e);
	   }
	   if (received == null){
		return StringTh.of("Error in getting a string from Erlang");
	   }
	   return StringTh.of(received.toString());
	}    
}
