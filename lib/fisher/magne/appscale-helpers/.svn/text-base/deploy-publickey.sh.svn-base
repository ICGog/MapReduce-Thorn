#!/bin/bash
for ip in `all-ips.sh`
do
    scp ~/.ssh/id_rsa.pub root@$ip:
    ssh root@$ip 'cat ~/id_rsa.pub >> ~/.ssh/authorized_keys; rm ~/id_rsa.pub'
done
