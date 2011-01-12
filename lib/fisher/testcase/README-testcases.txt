The test cases are not examples of good Thorn code. They are written to
torment the interpreter, not to show proper coding style. In particular, they
make extensive use of the Thorn self-test commands: ~!@eq(a,b)@!~ checks that
and b are equal, and, if not, fails in a way that the testing framework can
use nicely.  These commands will not help you unless you are trying to test
the interpreter.  
