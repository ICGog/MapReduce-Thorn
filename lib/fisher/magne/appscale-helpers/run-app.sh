#!/bin/bash
#set -x

# run app on appscale

if [ -z "$1" ];
    then
    echo "application dir is required try: run-app.sh {appdir}" 
    exit 1
fi

SCRIPT_DIR=`pwd`/`dirname $0`
FILE_NAME="transfer.tar.gz"
CONTROLLER_IP=`$SCRIPT_DIR/controller-ip.sh $SCRIPT_DIR`

HOST_NAME=root@$CONTROLLER_IP

cd $1
tar cfzv $FILE_NAME * --exclude='.svn'
scp -r $FILE_NAME $HOST_NAME:
rm $FILE_NAME

# copy ip.yaml to controller
scp $SCRIPT_DIR/ip.yaml $HOST_NAME:
ssh $HOST_NAME "/root/appscale-tools/bin/appscale-run-instances --ips ~/ip.yaml --table mongodb --file ~/$FILE_NAME --test -v" &
#appscale does: killall java
#that kills Thorn so we wait a bit
sleep 4
ssh $HOST_NAME '/root/appscale/AppServer_Thorn/runmemcache.sh /tmp/thornmemcache.log' &
ssh $HOST_NAME '/root/appscale/AppServer_Thorn/rundb.sh /tmp/thorndb.log' &
