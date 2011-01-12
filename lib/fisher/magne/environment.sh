#!/bin/bash
SERVER_PATH="$ABS_PATH/webserver"
WEBTH_PATH="$ABS_PATH/web"
THORN_PATH="$ABS_PATH/thorn"

WEBCHEEPER_PATH="$ABS_PATH/cheeper"

CLASSPATH="$THORN_PATH/fisher.jar:$THORN_PATH/junit.jar:$ABS_PATH/magnejava/lib/commons-fileupload-1.2.1.jar:$ABS_PATH/magnejava/bin"

files () {
    FILES=
    for filename in $1
    do
        FILES="$FILES$filename;" # ";".join(...)
    done;
    echo "${FILES%\;}"
}

# find the ip.
# this is important in distributed communication 
# since the sender ip is included in messages, and the one 
# replied to.
IP () {
    IP=`/sbin/ifconfig eth0 | grep 'inet a' | cut -d: -f2 | awk '{ print $1}'`
    if [ -z "$IP" ]
    then
        echo "localhost"
    else
        echo "$IP"
    fi
}

# NOTE: the ordering of the files matters in terms of imports!
WC=$(files "$WEBCHEEPER_PATH/*.thm")
WS=$(files "$SERVER_PATH/*.thm")
WT=$(files "$WEBTH_PATH/*.thm")

TESTMODULES="$ABS_PATH/test/unittest.thm"
MODULES="$WS;$WT;$WC"

th () { 
    java -classpath $CLASSPATH fisher.run.Thorn $* ; 
}

# Runs all test in the supplied dir. 
#
# Args:
#   1: test dir path
runtest () {
    for filename in "$@"
    do
        th -m "$MODULES;$TESTMODULES" -1 $filename
    done;
}