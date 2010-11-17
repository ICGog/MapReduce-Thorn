#!/bin/bash
# arg $1: log file

ABS_PATH_NAME=`echo $0 | sed "s#^\\([^/]\\)#${PWD}/\\1#"`
ABS_PATH=`dirname $ABS_PATH_NAME`

. $ABS_PATH/environment.sh

if [ -z $1 ]; then
    th -h `IP` -s $ABS_PATH/thorn-memcache/memcache.ths -p 11211
else
    th -h `IP` -s $ABS_PATH/thorn-memcache/memcache.ths -p 11211 >> $1 2>&1 &
fi

