package magne;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import com.sun.net.httpserver.HttpServer;

import fisher.runtime.Thing;
import fisher.runtime.ThingExtended;
import fisher.runtime.dist.ComponentThread;
import fisher.runtime.dist.SiteData;
import fisher.util.FisherException;

public class SimpleHTTPServer extends ThingExtended{
	
	private HttpServer server;
	private ExecutorService pool;

	public SimpleHTTPServer(Thing port) throws FisherException {
		this((int)port.asJavaInt(null), ComponentThread.mine());
	}

	public SimpleHTTPServer(int port, ComponentThread messagesGoTo) {
		super();
		try{
			// backlog, the number of simultaneous requests. 0 system default
			server = HttpServer.create(new InetSocketAddress(port), 0);
			server.createContext("/", new SimpleHTTPHandler(messagesGoTo));
			pool = Executors.newCachedThreadPool();
			server.setExecutor(pool); 
			server.start();
		}
		catch(IOException e){
			System.err.println("HttpServer exception");
			e.printStackTrace();
		}
	}

	public void stop(){
		server.stop(0);
		pool.shutdown();
	}
}
