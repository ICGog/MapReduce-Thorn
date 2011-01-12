#!/bin/bash
#set -x
ABS=`pwd`/`echo $0`
DIR=`dirname $ABS`
cd $DIR/../appscale-helpers/
CONTROLLER_IP=`$DIR/../appscale-helpers/controller-ip.sh`
scp $DIR/appscale-upload-app root@$CONTROLLER_IP:/root/appscale-tools/bin
scp $DIR/helperfunctions.rb root@$CONTROLLER_IP:/root/appscale/AppController
scp $DIR/common_functions.rb root@$CONTROLLER_IP:/root/appscale-tools/lib

