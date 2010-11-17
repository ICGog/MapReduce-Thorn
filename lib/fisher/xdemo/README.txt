The quick instructions for running the Fisher edition of Thorn,
as of May 29 2009.  

(Beware! These will change!)

STEP 1: Get Thorn

Get ahold of the necessary jar files: fisher.jar and junit.jar.  They are in
the SVN repository: svn://dumbo.cs.purdue.edu/Thorn/fisher
While you're at it, get the demo directory too, which includes some runnable 
demos, and the doc directory for some incomplete, inaccurate, and
ill-organized notes.
To get the whole thing
  svn co svn://dumbo.cs.purdue.edu/Thorn/fisher

STEP 2: Try It 

Try it out.  Go to .../demo/hello-world, and run java with these arguments (in
this order):
  * '-classpath .../fisher.jar;.../junit.jar'.  The ... is the path to that
    jar file.
  * 'fisher.run.Thorn'
  * '-f hello-world.th'

% cd $WHATEVER/demo/hello-world
$ java -classpath /Users/bard/thorn/fisherws/fisher/fisher.jar;/Users/bard/thorn/fisherws/fisher/junit.jar fisher.run.Thorn -f hello-world.th

That should print a traditional message on the console.

STEP 3: Shell Install

If you want to install Fisher-version Thorn in a Bash shell, add your system's
version of the following lines to your ~/.bashrc file.  You will need to
supply the proper path to the jar files.

  FISHERJARS=/Users/bard/thorn/fisherws/fisher/fisher.jar;/Users/bard/thorn/fisherws/fisher/junit.jar
  export FISHERJARS
  function th () { java -classpath $FISHERJARS fisher.run.Thorn $* ; }

After doing so (and rereading your bashrc, with "source ~/.bashrc", in each
shell that's currently running that needs to run that command), the shell
command 'th' will run thorn:

% th -f hello-world.th
Hello, world



STEP 4: Running the Cheeper demo

Add the following useful stuff to your .bashrc too: 
  FISHERDEMO=/Users/bard/thorn/fisherws/fisher/demo
  export FISHERDEMO

Go to the cheeper directory: 
  % cd $FISHERDEMO/cheeper

(Optional: If the checkout process messed permissions up, make sure
that the command files are executable: 
  % chmod +x run*
)

Start a terminal window for the server.  In this terminal window, run the
server: 
  % ./run-server
It should respond with:
  Cheeper server is up and running.
(And then it will sit and wait to serve commands.  It prints out various
messages as it runs.)

Start another terminal for the client.  In this one, run the client: 
  % ./run-client
The client will print a slogan, then ask you for your name and password.  
If you supply a new name, it will automatically register you with the server.
(This is called "security.")

The Cheeper command-line interface is pretty haphazard.  Type "/?" for help,
such as it is.
You can enter cheeps by just typing them; lines beginning with '/', '+', and
'-' are special.  Here's a sample run.  I typed "bard" and "meow" to log in; 
then: 
  * /?  -- to list the commands
  * I like cheese! -- my first chirp!
  * I like Varttina! -- my second chirp!
  * /a -- list all chirps in the system.
  * /t -- list the top five 
  * +1 -- to vote for chirp number 1.  Note that its votes have changed.
  * /t -- again, to list the top five.  #1 is on top.

For extra fanciness, start up another window, run a second client
with ./run-2-client, and have a chirpfest.  (Note: run-client and run-2-client
run the same code; they just use different ports.)

IBM-CD94F6CD58E:cheeper bard$ ./run-client
Cheeper by the dozen.
Who are you: bard
Password: meow
-- Welcome to Cheeper.  /? for help --
Chirp:/?
/q: quit client
/x: quit client and server
/a: print all chirps
/t: print the top five chirps
/?: this help message
+n: vote for chirp number n
-n: vote against chirp number n
/s; tell server to save
/r: tell server to reload from saved
Anything else is a chirp of your wisdom!
Chirp:I like cheese!
You chirped I like cheese!.
Chirp:I like Varttina!
You chirped I like Varttina!.
Chirp:/a
(1) I like cheese! [from bard, mod 1, +0/-0]
(2) I like Varttina! [from bard, mod 2, +0/-0]
Chirp:/t
(2) I like Varttina! [from bard, mod 2, +0/-0]
(1) I like cheese! [from bard, mod 1, +0/-0]
Chirp:+ 1
(1) I like cheese! [from bard, mod 3, +1/-0]
Chirp:/t
(1) I like cheese! [from bard, mod 3, +1/-0]
(2) I like Varttina! [from bard, mod 2, +0/-0]


------------------------------------------------------------------------------

Addendum: 

run-client takes a command line argument: a hostname of where the server
is running.  


=================================================================

Running the REPL: 

Add the following line to your .bashrc (plus the stuff above): 
  function threpl () { java -classpath $FISHERJARS fisher.run.REPL $* ; }
  
And then the command 'threpl' runs the Fisher-level interpreter repl.
It mostly just interprets one-line commands that you type to it.
(There are currently no provisions for multi-line commands.)
It has a couple of other controls: 
   #? -  help
   ## fn -- load the file named 'filename' as if you had typed it on one line.  This is 
            currently the only way to get multi-line commands.
            
The interpreter is currently pretty minimal.  It can't load modules
or use components.


