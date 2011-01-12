package fisher.runtime.lib.sox;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.Socket;
import java.net.SocketException;

import fisher.eval.Computer;
import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.runtime.StringTh;
import fisher.runtime.Thing;
import fisher.runtime.dist.ComponentThread;
import fisher.runtime.lib.http.HTTPMessage;
import fisher.runtime.lib.http.HTTPRequest;
import fisher.runtime.lib.http.MsgComp2HTTPSocket;
import fisher.runtime.lib.socketeer.MsgComp2StringSocket;
import fisher.runtime.lib.socketeer.ReplySocketTh;
import fisher.runtime.lib.socketeer.StringSocketThread;
import fisher.syn.core.Syntax;
import fisher.test.TestUtils;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;

public class AnythingSocketThread extends StringSocketThread {

	public final String inputStyle;

	public AnythingSocketThread(ComponentThread messagesGoTo, Socket socket, Thing protocol, String inputStyle) {
		super(messagesGoTo, socket, protocol);
		this.inputStyle = inputStyle;
	}

	@Override
	public void run() {
		if (inputStyle == null || "HTTP".equals(inputStyle))
			runHttp();
		else if ("LINE".equals(inputStyle))
			runLines();
		else
			Doom.runtimeNonThorn("Unknown input style: '" + inputStyle + "'. Choices are HTTP, LINE", null);

	}

	public void runHttp() {
		boolean goon = true;
		try {
			//			socket.setSoTimeout(ConnectionThread.SOCKET_TIMEOUT);
			final InputStream inputStream = socket.getInputStream();
			in = new BufferedReader(new InputStreamReader(inputStream));
			BufferedWriter out = new java.io.BufferedWriter(new java.io.OutputStreamWriter(socket.getOutputStream()));
			ReplySocketTh sr = new ReplySocketTh(out, socket);
			int sleepage = 10;
			while (goon && !siteData.allComponentsDone()) {
				//				THIS is where the HTTPification goes.
				//				String line = in.readLine();
				HTTPMessage request = HTTPRequest.read(in, sr);
				if (request != null) {
					MsgComp2HTTPSocket msg = new MsgComp2HTTPSocket(sr, request, null);
					messagesGoTo.mailbox.putLetterInQueue(msg);
				} else {
					/* Sometimes this code would go into a tight loop here and eat the whole CPU.
					 * (It would happen if wget were connecting to it, though not Firefox)
					 * So, if the request is null, wait a little while before looking again.
					 * Start off with 10 ms, and increase it slowly to 0.5 sec.  
					 * TODO: This is a complete hack based on me not really knowing what's going on,
					 * so if you know better, do the right thing.
					 * 
					 */
					try {
						Thread.sleep(sleepage);
					} catch (InterruptedException e) {
						// interrupted, which is fine.
					}
					sleepage = Math.min(sleepage + 10, 500); // wait a little while.

				}
			}
		} catch (SocketException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public void runLines() {
		boolean goon = true;
		InputStream inputStream;
		Syntax src = messagesGoTo.evaller.wholeSrc;
		try {
			//			socket.setSoTimeout(ConnectionThread.SOCKET_TIMEOUT);
			inputStream = socket.getInputStream();
			TestUtils.say(TestUtils.stringSocketRecv, this, "STRING RECV on : " + inputStream + " localPort= "
					+ socket.getLocalPort() + " port=" + socket.getPort() + " src=" + src + "");
			in = new BufferedReader(new InputStreamReader(inputStream));
			BufferedWriter out = new java.io.BufferedWriter(new java.io.OutputStreamWriter(socket.getOutputStream()));
			ReplySocketTh sr = new ReplySocketTh(out, socket);

			while (goon && !siteData.allComponentsDone()) {
				TestUtils.say(TestUtils.stringSocketRecv, this, "STRING RECV head of loop : " + inputStream + " src="
						+ src);
				String line = in.readLine();

				TestUtils.say(TestUtils.stringSocketRecv, this, "STRING RECV **GOT IT** : " + line + "   src=" + src);

				if (line != null) {
					MsgComp2StringSocket msg = new MsgComp2StringSocket(sr, StringTh.of(line), protocol, null);
					messagesGoTo.mailbox.putLetterInQueue(msg);
				}
			}
			TestUtils.say(TestUtils.stringSocketRecv, this, "STRING RECV after loop : " + inputStream + " src=" + src
					+ " goon=" + goon + " !siteData.allComponentsDone()=" + (!siteData.allComponentsDone()));
		} catch (SocketException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
