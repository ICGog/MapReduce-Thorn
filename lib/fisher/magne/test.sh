#!/bin/bash
# set -x
ABS_PATH_NAME=`echo $0 | sed "s#^\\([^/]\\)#${PWD}/\\1#"`
ABS_PATH=`dirname $ABS_PATH_NAME`

# cp /home/dam/Dropbox/ibm/thorn/Thorn/fisher/fisher.jar thorn

. $ABS_PATH/environment.sh

# start the memcache server 
xterm -e java -classpath $FISHERJARS fisher.run.Thorn -s $ABS_PATH/memcache/memcache.ths -p 11211 &
MEM_PID="$!"

# start the datastore server
xterm -e java -classpath $FISHERJARS fisher.run.Thorn -s $ABS_PATH/datastore/datastore.ths -p 11200 &
DB_PID="$!"


if [ -n "$1" ]; 
then 
    runtest $@ 
else
    # webth
    runtest $ABS_PATH/web/test/*test*.th
    # memcache
    runtest $ABS_PATH/memcache/test/*test*.th
    # webceheeper
    runtest $ABS_PATH/cheeper/test/*test*.th
    # datastore
    runtest $ABS_PATH/datastore/test/*test*.th
fi

# stop the memcache and datastore servers
kill "$MEM_PID"
kill "$DB_PID"