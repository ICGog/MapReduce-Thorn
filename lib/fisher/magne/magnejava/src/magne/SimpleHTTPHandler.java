package magne;



import java.io.IOException;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;

import fisher.runtime.dist.ComponentThread;

public class SimpleHTTPHandler implements HttpHandler {

	public final ComponentThread messagesGoTo;
	
	public SimpleHTTPHandler(ComponentThread messagesGoTo) {
		super();
		this.messagesGoTo = messagesGoTo;
	}

	public void handle(HttpExchange t) throws IOException {

		SimpleHTTPRequest request = new SimpleHTTPRequest(t);
        
        MsgSimpleHttpRequest msg = new MsgSimpleHttpRequest(request);
		messagesGoTo.mailbox.putLetterInQueue(msg);
    }
}