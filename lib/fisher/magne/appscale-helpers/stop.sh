#!/bin/bash
ABS=`echo $0`
cd `dirname $ABS`
CONTROLLER_IP=`controller-ip.sh`
ssh "root@$CONTROLLER_IP" '/root/appscale-tools/bin/appscale-terminate-instances --ips /root/ip.yaml'
# kill logs...
ssh root@$CONTROLLER_IP 'killall tail'
#ssh root@$CONTROLLER_IP 'killall java'
killall xterm