package fisher.runtime.lib.sox;

import java.io.IOException;
import java.net.Socket;
import java.net.SocketException;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.runtime.BoolTh;
import fisher.runtime.IntTh;
import fisher.runtime.Thing;
import fisher.runtime.builtInFun.PrintlnBIF;
import fisher.runtime.dist.ComponentThread;
import fisher.runtime.dist.ConnectionAcceptingThread;
import fisher.runtime.lib.socketeer.StringSocketThread;
import fisher.runtime.lib.socketeer.StringSocketeer;
import fisher.test.TestUtils;
import fisher.util.FisherException;

public class ListenOn extends StringSocketeer {

	public Thing protocol;
	public final String inputStyle;

	public ListenOn(int port, ComponentThread messagesGoTo, String endOfStringRegexp, Thing protocol) {
		super(port, messagesGoTo, endOfStringRegexp);
		this.protocol = protocol;
		this.inputStyle =  SoxUtil.inputStyle(protocol);
	}

	public ListenOn(Thing portth, Thing protocol) throws FisherException {
		super(portth);
		this.protocol = protocol;
		this.inputStyle =  SoxUtil.inputStyle(protocol);
		
	}
	
	public BoolTh eq(Thing other) {
		if (other instanceof ListenOn) {
			ListenOn ns = (ListenOn) other;
			return BoolTh.of(this.equals(other) && this.protocol == ns.protocol);
		}
		else return BoolTh.False;		
	}
	
	public IntTh th_hashcode() {
		return IntTh.of(this.hashCode());
	}
	
	@Override
	public Thing protocol(){return protocol;}

	@Override
	public void run() {
		try {
			serverSocket = ConnectionAcceptingThread.acquireServerSocket(port, 0, ConnectionAcceptingThread.backoffs);
			serverSocket.setSoTimeout(ConnectionAcceptingThread.ServerSocketTimeout);
			while (!site.allComponentsDone()) {
				try {
					TestUtils.say(false, this, "Head of loop");
					Socket newConnection = serverSocket.accept();
					
					AnythingSocketThread sst = new AnythingSocketThread(messagesGoTo,  newConnection, this.protocol(), inputStyle);
					sst.start();
				} catch (IOException e) {
					// Normal Timeout
				}
			}// while
		} catch (SocketException e) {
			System.err.println("Oh, dear, in StringSocketeer");
			e.printStackTrace();
		} finally {
			try {
				//whinge("ConnectionAcceptingThread -- trying to kill socket for " + site);
				serverSocket.close();
				//whinge("OK, closed it.");
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}
	protected StringSocketThread createSuitableSocketThread(Socket newConnection) {
		return new AnythingSocketThread(messagesGoTo,  newConnection, this.protocol(), SoxUtil.inputStyle(this.protocol()));
	}
}
