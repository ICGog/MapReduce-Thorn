===============================================================================
Thorn Interpreter v0.5 Distribution
===============================================================================

STEP 1: Install Thorn
---------------------

In the following, let THORNROOT designate the directory connecting the
thorn interpreter ("fisher") distribution (i.e., the directory
containing this file).  Fisher is implemented by the Java class files
in THORNROOT/classes, which have been compiled for Java 1.6.

To set up your system to run thorn, proceed as follows:

- Define and export the shell variable THORNJARS, like so:

  export THORNJARS=THORNROOT/classes/fisher.jar;THORNROOT/classes/junit.jar

- Add the directory THORNROOT/bin to your search path, e.g.,

  export PATH=$PATH:THORNROOT/bin

The bin directory contains two short shell script, ('th' and 'threpl') for
invoking Java/fisher from the command line.  'th' is the non-interactive
thorn interpreter; 'threpl' is the interactive thorn read-eval-print-loop
interpreter.  The script for 'th' is simply:

  #!/bin/bash
  java -classpath $THORNJARS fisher.run.Thorn $*

If you prefer, you can start Java directly and bypass the scripts.

STEP 2: Test Thorn 
------------------

Navigate to THORNROOT/demo/hello-world, and execute

  th -f hello-world.th

If installation is successful, this will print the traditional message
on the console


STEP 3: Experiment
------------------

The directory THORNROOT/demo contains a number of demo thorn applications.
Each application has a README file, and most have shell scripts of the
form run-... to simplify startup (type ./run-... to invoke).  Try them!

===============================================================================
More Details
===============================================================================

th: command line arguments and options
--------------------------------------

Typing

  th -m modseq -f foot.th

runs the thorn source file foo.th as a "simple script"; such scripts may not
use thorn concurrency features (message send/receive, spawn, etc.). 'modseq'
is a semicolon-separated list of file names of thorn module (.thm) files.
In general, there may be many -m arguments.

Typing

  th -m modseq -s foo.th -p port -h host

runs the top-level spawn command in foo.th as a component, where:

  port = a valid port number; the interpreter will listen for incoming messages
         from other thorn sites on this port

  host = a valid IP address for the host on which the interpreter is running;
         this is only required when communicating with thorn components
         running on other IP hosts

Typing

  th -m modseq -sf foo.th

puts the code in foo.th into a component, and spawns that component

The complete command syntax for th is as follows:

   th -m moduleFileNamesSepBySemicolons -m moreOfThem
      [-s thornFileToSpawn.th | -f thornFileToRun.th]
      -p port -h host [-np | --noprint] -- thornArgs

where

  thornArgs = space-separated list of strings to be passed to the
              program as arguments

  np = suppress printing


threpl: minimal thorn interactive interpreter
---------------------------------------------

threpl mostly just interprets one-line commands that you type to it; there
are currently no provisions for multi-line commands.  It also can't load
modules or use components.

It has a couple of other controls:

   #?: help

   ## fn: load the file named 'fn' as if you had typed it on one
          line.  This is currently the only way to get multi-line commands.
            
===============================================================================
Documentation
===============================================================================

The directory THORNROOT/doc contains:

- thorn-oopsla-2009.pdf: OOPSLA '09 paper introducing thorn

- thorn-langref-draft-05.pdf: Draft thorn language spec

===============================================================================
Comments and Questions
===============================================================================

Post comments and questions to the thorn-users mailing list by signing up
here:

  http://lists.wrigstad.com/mailman/listinfo/thorn-users
