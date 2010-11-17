#!/bin/bash

# run app on appscale

if [ -z "$1" ];
    then
    echo "application dir is required try: run-app.sh {appdir}" 
    exit 1
fi
# ../webcheeper/
# remove ../ and / now it will only work from ./magne
APPNAME=$1
SCRIPT_DIR=`pwd`/`dirname $0`
FILE_NAME="transfer.tar.gz"
CONTROLLER_IP=`$SCRIPT_DIR/controller-ip.sh $SCRIPT_DIR`

HOST_NAME=root@$CONTROLLER_IP

# killall thorn process server processes. Should be done by appscale???
for IP in `$SCRIPT_DIR/server-ips.sh $SCRIPT_DIR`
do
    ssh root@$IP "ps axo pid,command | egrep -e '^.*fisher.jar.*$' | awk '{ print \$1}' | xargs kill -9"
done

# tar and transfer
cd $1
tar cfzv $FILE_NAME * --exclude='.svn'
scp -r $FILE_NAME $HOST_NAME:
ssh $HOST_NAME "/root/appscale-tools/bin/appscale-remove-app --appname $APPNAME"
rm $FILE_NAME


# startup the app again
ssh $HOST_NAME "/root/appscale-tools/bin/appscale-upload-app --file ~/$FILE_NAME"

# FIXME: the app is first ready after the second time doing this???