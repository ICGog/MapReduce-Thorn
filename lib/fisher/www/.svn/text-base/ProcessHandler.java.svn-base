
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package www;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.Map;
import java.util.Queue;
import java.util.Random;
import java.util.StringTokenizer;
import java.util.Timer;
import java.util.TimerTask;

public  class  ProcessHandler implements Runnable  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
    private Map<String, SessionHandler> _ps;
    
    private ProcessBuilder pb;
    private Thread t;
    
    public static int WEB_TIMEOUT = 30*1000;
    
    public ProcessHandler () {
        _ps = new ConcurrentHashMap<String, SessionHandler> ();
        
        pb = new ProcessBuilder ("bin/repl", "--safer");
        pb.redirectErrorStream (true);
        
        t = new Thread (this);
        t.start ();
    }
    
    /**
     * The main thread of ProcessHandler, responsible for killing web sessions that are never used.
     * If a process's SessionHandler  has not recieved a message from a session since the last check
     * which occurs every WEB_TIMEOUT milliseconds, the SessionHandler's stop method is invoked.
     */
    public void run () {
        while (true) {
            try {
                Thread.sleep (WEB_TIMEOUT);
            } catch (InterruptedException te) { }
            
            for (String key : _ps.keySet ()) {
                SessionHandler sh = _ps.get (key);
                
                if (!sh.hasBeenVisited ()) {
                    sh.stop ();
                    _ps.remove (key);
                }
            }
        }
    }
    
    /**
     * Visits or attempts to create a session of the thorn compiler. It also creates a unique sessionId
     * if the session did not previously exist. Visiting a session prevents it from being deleted on the 
     * next timeout check.
     *
     * @param session  The sessionId to be visited if it exists.
     * @return         The argument session if a visit occured, a new unique sessionId String if a new session was
     *                 or an error string which is not in the format of a sessionId if a creation failed.
     */
    public String getSession (String session) {
        if (!_ps.containsKey (session)) {
            SessionHandler sh;
            
            do {
                session = createToken ();
            } while (_ps.containsKey (session));
            
            try {
                sh = new SessionHandler (pb.start(), session);
            } catch (IOException e) {
                return "Failed to create SessionHandler";
            }
            
            _ps.put (session, sh);
        } else {
            SessionHandler sh = _ps.get (session);
            sh.visit ();
        }
        
        return session;
    }
    
    /**
     * Forwards a command to the appropriate session's sessionHandler. The appropriate session
     * is found from the sessionId given
     *
     * @param s        the socket which results of the command should be read/written from/to.
     * @param session  the sessionId of the session to recieve the command.
     * @param cmd      the Thorn code to be evaluated by the session.
     */
    public void handleCommand (Socket s, String session, String cmd) {
        SessionHandler sh = _ps.get (session);
        
        sh.handleCommand (s, cmd);
    }

    /**
     * Randomly generates an 8 character alphanumeric string, to be used as a sessionId.
     * Only uppercase letters and the digits 0-9 are used.
     *
     * @return  a random 8 character alphanumeric string
     */
    private static String createToken () {
        String token = "";
        String charset = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
        Random r = new Random ();
        
        int i = 8;
        while (i-- > 0) {
            token += (char) charset.charAt (r.nextInt (36));
        }
        
        return token;
    }

}

class SessionHandler implements Runnable {
    private Timer timeout;
    
    private Queue<Connection> queue;
    private boolean isRunning;
    private boolean to;
    private Thread t;
    private Process p;
    private String session;
    private boolean visited;
    
    private InputStream is;
    private OutputStream os;

    public final static String PROMPT = "Thorn: \n";
    
    /**
     * Creates a new session handler for a process. The session handler is in charge of
     * writing output from a process, and killing processes when a session is no longer active.
     *
     * @param p        the thorn process to monitor
     * @param session  the unique sessionId for this session
     *
     */
    SessionHandler (Process p, String session) {
        queue = new ConcurrentLinkedQueue<Connection> ();
        visited = true;
        this.p = p;
        this.session = session;
        
        is = p.getInputStream ();
        os = p.getOutputStream ();
        isRunning = true;
        t = new Thread (this);
        t.start ();
        
        timeout = new Timer ();
        to = false;
    }
    

    /**
     * The main thread of sessionHandler. Reads commands from the input queue and then passes them on to
     * the running thorn process. Also writes output to webpage, after eliminating the terminal prompt
     * from the output. All output before the first prompt is ignored.
     */
    public void run () {
        try {
            String res = "";
            while (true) {
                res += (char) is.read ();
                if (res.endsWith (PROMPT)) {
                    break;
                }
            }
            
            while (isRunning) {
                Connection conn = queue.poll ();
                
                if (conn == null) {
                    try {
                        Thread.sleep (1000000);
                    } catch (InterruptedException ie) { }
                } else {
                    TimerTask tt = new ProcessTimeout (this);
                    timeout.schedule (tt, 10000);
                    char[] c = new char[PROMPT.length()];
                    
                    PrintWriter in = new PrintWriter (p.getOutputStream ());
                    in.println (conn.cmd);
                    in.flush ();
                    
                    OutputStream sos = null;
                    String buff = session + "\n";
                    
                    try {
                        sos = conn.conn.getOutputStream ();
                        for (int i = 0; i < c.length; i++)
                            c[i] = (char) is.read();

                        boolean wroteOutput = false;
                        while (!(checkPrompt(c) && is.available () == 0) && isRunning) {
                            buff += c[0];
                            wroteOutput = true;
                            
                            for (int i = 0; i < c.length - 1; i++)
                                c[i] = c[i + 1];
                            
                            while (is.available () == 0) {
                                try {
                                    Thread.sleep (100);
                                } catch (InterruptedException ie) {
                                    continue;
                                }
                            }
                            
                            c[c.length - 1] = (char) is.read();
                        }
                        
                        //Needed to force output on web, for things like class defs that return nothing
                        if (!wroteOutput){
                            buff += "null\n";
                        }
                        
                        if (to) {
                            buff += "Timeout Occured\n";
                        }
                    } catch (IOException ioe) {
                        if (to) {
                            buff += "Timeout Occured\n";
                        }
                    }
                    
                    if (sos != null) {
                        sos.write (("Content-Length: "+buff.length ()+"\n").getBytes ());
                        sos.write ('\n');
                        sos.write (buff.getBytes ());
                        sos.flush ();
                        conn.conn.close ();
                    }
                                    
                    tt.cancel ();
                }
            }
        } catch (IOException e) {
            timeout.cancel ();
        }
        
        kill ();
    }

    public boolean checkPrompt(char[] c){
        for (int i = 0; i < c.length; i++)
            if (c[i] != PROMPT.charAt(i)) return false;
        return true;
    }

    
    /**
     * Stops the run cycle of the sessionHandler, interrupting the thread and then calling this.kill()
     */
    public void stop () {
        isRunning = false;
        t.interrupt ();
        kill ();
    }
    
    /**
     * Adds a command to the queue of commands to be forwarded by run() to the thorn program.
     *
     * @param s    the socket to write output to
     * @param cmd  the command string to be evaluated by the thorn compiler.
     */
    public void handleCommand (Socket s, String cmd) {
        queue.add (new Connection (s, cmd));
        t.interrupt ();
    }
    
    /**
     * Sets the session's visited flag, preventing it from being killed at the next inactivity check.
     */
    public void visit () {
        visited = true;
    }
    
    /**
     * Checks to see if a session's visit method has been called, clearing the visited flag in the process.
     *
     * @return the old value of the visited flag
     */
    public boolean hasBeenVisited () {
        if (isRunning) {
            boolean hbv = visited;
            visited = false;
            return hbv;
        } else {
            return false;
        }
    }
    
    /**
     * Kills any processes that contain the sessionHandler's session id. It is assumed that
     * no other processes on the system will have anythign that resembles that sessionId,
     * as almost every process name is written with at least one lowercase letter, which is invalid
     * in a sessionId.
     */
    public void kill () {
        Runtime rt = Runtime.getRuntime();
        
        try {
            is.close ();
            os.close ();
            
            Process ps = rt.exec ("ps -Af");
            InputStream is = ps.getInputStream ();
            BufferedReader in = new BufferedReader(new InputStreamReader(is));
            String line = null;
            
            while ((line = in.readLine ()) != null) {
                if (line.contains (session)) {
                    StringTokenizer st = new StringTokenizer (line);
                    st.nextToken (); // username
                    
                    String cmd[] = { "kill", "-9", st.nextToken () };
                    rt.exec (cmd).waitFor ();
                }
            }
        } catch (IOException e) {
        } catch (InterruptedException e) { }
    }
    
    public void fireTimeout () {
        to = true;
        stop ();
    }
}

/**
 * Timer task that gets scheduled to run during the execution of a command.
 * If the timeout gets fired, then the process is killed and the client is
 * notified.
 */
class ProcessTimeout extends TimerTask {
    private SessionHandler sh;
    public ProcessTimeout (SessionHandler sh) {
        this.sh = sh;
    }
    
    public void run () {
        sh.fireTimeout ();
    }
}

class Connection {
    Socket conn;
    String cmd;
    Connection (Socket conn, String cmd) {
        this.conn = conn;
        this.cmd = cmd;
    }
}
