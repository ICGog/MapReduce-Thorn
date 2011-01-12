window.onload = termOnLoad;

String.prototype.trim = function() { return this.replace(/^\s+|\s+$/g, ''); }
String.prototype.endsWith = function(str) {return (this.match(str+"$")==str)}

var waiting = true;
var disconnected = false;
var hasSeenHelp = 0;

var xmlhttp;
var xmlhttpw;
var dispbuff = "";
var session = "00000000";

/**
 * Checks if a given command has its comments, brackets, and string literals closed.
 * This is used as a guide for checking if a user is done entering a command when he or she 
 * presses enter.
 *
 * @param cmd  the command to be checked
 * @return     true if all comments, brackets, and strings are closed, false otherwise.
 */
function isCommandComplete (cmd)
{
    var depth = 0;
    
    for (i = 0; i < cmd.length; i++) {
        if (cmd[i] == '#') {
            for (; cmd[i] != '\n'; i++);
        } else if (cmd[i] == '{' || cmd[i] == '(' || cmd[i] == '\"') {
            if (cmd[i] == '(' && cmd[i+1] == '#') {
                for (i++; i < cmd.length - 1; i++) {
                    if (cmd[i] == '#' && cmd[i+1] == ')')
                        break;
                }
                
                if (!(cmd[i] == '#' && cmd[i+1] == ')'))
                    return false;
                
                i++;
            } else if (cmd[i] == '\"' && (i == 0 || cmd[i-1] != '\\')){
                for (i++; i < cmd.length - 1; i++) {
                    if (cmd[i] == '\"' && cmd[i-1] != '\\')
                        break;
                }
            } else {
                depth++;
            }
        } else if (cmd[i] == '}' || cmd[i] == ')') {
            depth--;
        }
    }
    
    if (depth <= 0) {
        return true;
    }
    
    return false;
}


/**
 * Initalizes the terminal, and sends a dummy request to the compiler to perform the initalization steps.
 */
function termOnLoad ()
{
    updateTerm ();
    
    xmlhttp = new XMLHttpRequest ();
    xmlhttp.onreadystatechange = initStateChanged;
    
    var url = "/thorn?";
    url += "session=" + session;
    url += "&cmd=\"Welcome to Thorn (Fisher Version)\";";
    
    xmlhttp.open ("GET", url, true);
    xmlhttp.send (null);
    
    document.getElementById("termin").value = "Welcome to the Fisher web demo! When the demo loads, click here to type or paste your code. \nWhen finished, press enter and your result will be shown above."
}

/**
 * Sends a dummy request to the web server to indicate that the session is still open.
 * This prevents it from being closed by inactivity checkers.
 */
function watchdogTimer ()
{
    xmlhttpw = new XMLHttpRequest ();
    xmlhttpw.onreadystatechange = watchdogStateChanged;
    
    var url = "/thorn?";
    url += "session=" + session;

    try {
        xmlhttpw.open ("GET", url, true);
        xmlhttpw.send (null);
    } catch (e) {
        handleDisconnect();
        return;
    }
    
    if (!disconnected) {
        setTimeout (watchdogTimer, 10000);
    }
}

/**
 * Generates a command that is on one line to allow it to be sent in the url.
 * Comments are removed, otherwise they would affect the entire remainder of the code
 * when the new lines are removed.
 *
 * @param cmd  the string to be flattened
 * @return     a new string that is the original text without thorn comments or newlines.
 *
 */
function flattenCmd (cmd)
{
    var text = "";

    for (i = 0; i < cmd.length; i++) {
        if (cmd[i] == '#') {
            for (; cmd[i] != '\n'; i++);
        } else if (cmd[i] == '(' || cmd[i] == '\"') {
            if (cmd[i] == '(' && cmd[i+1] == '#') {
                for (i++; i < cmd.length - 1; i++) {
                    if (cmd[i] == '#' && cmd[i+1] == ')')
                        break;
                }
                i++;
            } else if (cmd[i] == '\"' && (i == 0 || cmd[i-1] != '\\')){
                text += cmd[i];
                for (i++; i < cmd.length - 1; i++) {
                    if (cmd[i] != '\n')
                        text += cmd[i];
                    if (cmd[i] == '\"' && cmd[i-1] != '\\')
                        break;
                }
            } else if (cmd[i] == '(') {
                text += cmd[i];
            }
        } else {
            if (cmd[i] != '\n'){
                text += cmd[i];
            }
        }
    } //For
    return text;
}

/**
 * Detects when the evaluate button is pressed, and attempts to submit the code to the server.
 * The code will only be submitted if it is complete as determined by isCommandComplete.
 *
 * @param evt  the click event.
 */

function evalClicked(evt)
{
    var msgbox = document.getElementById ("inputmsg");
    if (isCommandComplete (document.getElementById ("termin").value)) {
        msgbox.innerHTML = "<font color=#CC3300><i>(Evaluating)</i></font>" 
        onSubmit ();
        evt.returnValue = false;
        return false;
    }
    else {
        msgbox.innerHTML = "<i>(Close all comments, brackets, parenthesis, and quotes before submission)</i>";
    }
}

/**
 * Clears the help dialogue that appears by default in the inputbox, if it is currently displayed.
 * 
 * @param evt  the mouse down event
 */
function onMouseDown(evt)
{
    var termin = document.getElementById("termin");
    if (!hasSeenHelp && !termin.disabled){
      termin.value = "";
      termin.focus();
      hasSeenHelp = 1;
    }
}


/**
 * Submits the code in the input textbox to the webserver.
 * This causes the page to go into a waiting state for a response, disabling 
 * the input box.
 */
function onSubmit ()
{
    var termin = document.getElementById ("termin");
    var evalButton = document.getElementById("evalButton");
    var textin = termin.value;
    if (hasSeenHelp)
        termin.value = "";

    if (textin == "bye\n" || textin == "quit\n" || textin == "exit\n"){
        handleDisconnect();
        document.getElementById("inputmsg").innerHTML = "<font color=#CC3300><i>Session ended</i></font>"
        return;
    }
    
    dispbuff += "\nCommand:\n" + textin;
    
    updateTerm ();
    
    var totalcmd = flattenCmd (textin);
    try {
        xmlhttp = new XMLHttpRequest ();
        xmlhttp.onreadystatechange = stateChanged;
            
        var url = "/thorn?";
        url += "session=" + session;
        url += "&cmd=" + escape (totalcmd);
            
        xmlhttp.open ("GET", url, true);
        xmlhttp.send (null);

        evalButton.disabled = true;        
        termin.disabled = true;
    } catch (err) {
        txt="There was an error on this page.\n\n";
        txt+="Error message: " + err.message + "\n\n";
        txt+="Error name: " + err.name + "\n\n";
        txt+="Click OK to continue.\n\n";
        alert(txt);
    }
}

/**
 * Handles the inital xmlhttp stateChanged events created during the page load.
 */
function initStateChanged ()
{
    if (xmlhttp.readyState == 4) {
        if (xmlhttp.status == 200) {
            session = xmlhttp.responseText.substring (0, 8);
            
            dispbuff += xmlhttp.responseText.substring (9);
            
            updateTerm ();
        } else {
            handleDisconnect();
        }

        document.getElementById ("inputmsg").innerHTML = "Ready";
        
        var termin = document.getElementById ("termin");
        termin.disabled = false;
        document.getElementById("evalButton").disabled = false;
        
        setTimeout (watchdogTimer, 10000);
    }
}


/**
 * Handles the majority of responses from the web server, updating the terminal to display the result.
 * It also disables the webpage in the event of recieving a timeout error, indicating the sessions process
 * on the webserver has been destroyed.
 */
function stateChanged ()
{
    if (!disconnected) {
        if (xmlhttp.readyState == 4) {
            if (xmlhttp.status == 200) {
                session = xmlhttp.responseText.substring (0, 8);
                response = xmlhttp.responseText.substring(9);
                
                if (response.endsWith("Timeout Occured\n")){
                    dispbuff += "Your session has timed out, possibly because of code that took too long to evaluate. \nPlease refresh your browser to start a new session";
                    handleDisconnect();
                    return;
                }

                if (response.length != 0){
                    dispbuff += "\nResult:\n";
                    dispbuff += response;
                
                    var termin = document.getElementById ("termin");
                    termin.disabled = false;
                    termin.blur ();
                    termin.focus ();
                    document.getElementById("evalButton").disabled = false;

                    document.getElementById ("inputmsg").innerHTML = "Ready";
                
                    updateTerm ();
                }
            } else {
                handleDisconnect();
            }
        }
    }
}

/**
 * Performs the default actions of being disconnected. These are currently
 * disabling input, and displaying a disconnected message.
 */
function handleDisconnect()
{
    if (!disconnected){
        waiting = true;
                
        var termin = document.getElementById ("termin");
        termin.disabled = true;
        document.getElementById("evalButton").disabled = true;

        var inputmsg = document.getElementById("inputmsg");
        inputmsg.innerHTML = "<font color=#CC3300><b><i>Disconnected from host</i></b></font>";
                
        disconnected = true;
        updateTerm ();
    }
}

/**
 * Updates sessionid from a handled watchdog xmlhttprequest.onreadystatechanged calls.
 */
function watchdogStateChanged ()
{
    if (!disconnected) {
        if (xmlhttpw.readyState == 4) {
            if (xmlhttpw.status == 200) {
                session = xmlhttpw.responseText.substring (0, 8);
            } else {
                handleDisconnect();
            }
        }
    }
}

/**
 * Updates the output on the terminal to reflect the contents of dispbuff.
 */
function updateTerm ()
{
    var term = document.getElementById ("termout");
    term.innerHTML = dispbuff;
    term.scrollTop = term.scrollHeight;
}

