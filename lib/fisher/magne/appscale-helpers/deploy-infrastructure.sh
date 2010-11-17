#!/bin/bash
# Deploy Thorn server, webth, memcache, datastore, ...
set -x
ABS=`pwd`/`dirname $0`
MAGNE_DIR=$ABS/..
cd $MAGNE_DIR
tar cfzv $ABS/transfer.tar.gz * --exclude="*svn*"
cd $ABS

for IP in `all-ips.sh`
do
    ssh root@$IP "rm -rf /root/appscale/AppServer_Thorn/"
    ssh root@$IP "mkdir /root/appscale/AppServer_Thorn/"

    scp transfer.tar.gz root@$IP:/root/appscale/AppServer_Thorn/
    ssh root@$IP "cd /root/appscale/AppServer_Thorn/ ; tar xfv transfer.tar.gz"
done
rm transfer.tar.gz