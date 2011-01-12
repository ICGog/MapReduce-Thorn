#!/bin/bash
# set -x
if [ -z "$1" ];
    then
    echo "application dir is required try: dev_appserver.sh {appdir}" 
    exit 1
fi

# if absolute path not supplied to $1, put it in
APP_ABS_PATH=`echo $1 | sed "s#^\\([^/]\\)#${PWD}/\\1#"`
ABS_PATH=`dirname $0`

. $ABS_PATH/environment.sh

if test ! -e "$APP_ABS_PATH/main.th"; 
then 
    echo "main.th doesn't exist in $APP_ABS_PATH/main.th"
    exit 1
fi

# default values
thornport="7080"
serverport="8080"

# Get the ip address of this machine

memcacheaddress="thorn://localhost:11210"
datastoreaddress="thorn://localhost:11200"

shift # shift away the appdir argument

while getopts t:s:m:d: opt
do
    case "$opt" in
      t)  thornport="$OPTARG";;
      s)  serverport="$OPTARG";;
      m)  mecacheaddress="$OPTARG";;
      d)  datastoreaddress="$OPTARG";; 
        \?)		# unknown flag
      	  echo "usage: $0 [-d datastoreaddress] [-m memcacheaddress][-t thornport] [-s serverport] "
	  exit 1;;
    esac
done

WS=$(files "$SERVER_PATH/*.thm")
WT=$(files "$WEBTH_PATH/*.thm")
WA=$(files "$APP_ABS_PATH/*.thm")
MODULES="$WS;$WT;$WA"

th -p $thornport -m $MODULES -h `IP` -1 $APP_ABS_PATH/main.th -- $appdir --port=$serverport --memcacheaddress=$memcacheaddress --datastoreaddress=$datastoreaddress
