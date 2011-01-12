#!/bin/bash
set -x
if [ -z "$1" ];
    then
    echo "appname is required try: log-appscale.sh {appname}" 
    exit 1
fi

SCRIPT_DIR=`pwd`/`dirname $0`
CIP=`$SCRIPT_DIR/controller-ip.sh $SCRIPT_DIR`

xterm -e ssh "root@$CIP" "tail -f /tmp/thornmemcache.log" &
xterm -e ssh "root@$CIP" "tail -f /tmp/thorndb.log" &
xterm -e ssh "root@$CIP" "tail -f /tmp/$CIP.log" &

for ip in `$SCRIPT_DIR/server-ips.sh $SCRIPTDIR`
do
    xterm -e ssh "root@$ip" "tail -f /tmp/$ip.log" &
done

LOG="log(){
    if test -e /var/apps/$1/log/server.log ;
    then 
        tail -f /var/apps/$1/log/server.log 
    else 
        echo 'waiting for webserver log to exist...'
        sleep 2 
        log 
    fi 
}
log"

for ip in `$SCRIPT_DIR/server-ips.sh $SCRIPT_DIR`
do
    xterm -e ssh "root@$ip" "$LOG" &
done

