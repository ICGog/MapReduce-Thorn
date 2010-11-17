
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
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;

public  class  WebServer  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
      
    public static void main(String args[]) {
        ServerSocket ss = null;
        ProcessHandler ph = new ProcessHandler();
        
        try {
            ss = new ServerSocket(4081);
        } catch (IOException ioe) {
            return;
        }
        while (true) {
            try {
                Socket s = ss.accept();
                
                BufferedReader sInput = new BufferedReader(new InputStreamReader(s.getInputStream()));
                OutputStream os = s.getOutputStream();
                String[] parts = sInput.readLine().split(" ");
                
                if (parts[0].equals("GET")) {
                    if (parts[1].equals("/")) {
                        parts[1] = "/index.html";
                    }
                    
                    if (parts[1].startsWith("/thorn")) {
                        if (parts[1].indexOf("?") != -1) {
                            String[] thornString = parts[1].split("\\?");
                            
                            if (thornString.length != 2) {
                                s.close();
                                continue;
                            } else {
                                String[] cmds = thornString[1].split("&");
                                
                                String session = null;
                                String code = null;
                                
                                for (String cmd : cmds) {
                                    String[] keyval = cmd.split("=", 2);
                                    if (keyval[0].equals("cmd")) {
                                        code = unescape (keyval[1]);
                                    } else if (keyval[0].equals("session")) {
                                        session = keyval[1];
                                    }
                                }
                                
                                session = ph.getSession(session);
                                String resp = parts[2] + " 200 OK\n";
                                resp += "Connection: close\n";
                                os.write(resp.getBytes());
                                
                                if (code != null) {
                                    ph.handleCommand (s, session, code);
                                } else {
                                    os.write ('\n');
                                    os.write ((session + "\n").getBytes());
                                    s.close();
                                }
                            }
                        }
                    } else {
                        parts[1] = "www" + parts[1];
                    
                        File f = new File(parts[1]);
                        String resp = parts[2] + " ";
                        if (f.isFile()) {
                            resp += "200 OK\n";
                            resp += "Content-Length: " + f.length() + "\n";
                            resp += "Connection: close\n";
                            resp += "\n";
                            os.write(resp.getBytes());

                            FileInputStream fis = new FileInputStream(f);
                            byte[] data = new byte[512];
                            int len;
                            while ((len = fis.read(data)) != -1) {
                                os.write(data, 0, len);
                            }
                            s.close ();
                        } else {
                            resp += "404 Not Found\n";
                            resp += "Connection: close\n";
                            resp += "\n";

                            os.write(resp.getBytes());
                            s.close();
                        }
                    }
                } else {
                    String resp = parts[2] + " 404 Not Found\n";
                    resp += "Connection: close\n";
                    resp += "\n";
                    os.write(resp.getBytes());
                    s.close();
                }
            } catch (IOException ioe) {
                System.out.println ("Main IO Exception");
            }
        }
    }

    /**
     * Creates a new String identical to the input String, but with any % denoted
     * escape characters replaced.
     *
     * @param input  the string to replace escape characters from
     * @return a new string with the escape characters replaced
     *
     */
    private static String unescape (String input) {
        String res = "";
        for (int i = 0; i < input.length(); i++) {
            if (input.charAt (i) == '%') {
                if (input.charAt (i+1) == '%') {
                    res += "%";
                    i++;
                } else {
                    res += Character.toString ((char) Integer.parseInt (input.substring (i+1, i+3), 16));
                    i += 2;
                }
            } else {
                res += input.charAt (i);
            }
        }

        return res;
    }


}
