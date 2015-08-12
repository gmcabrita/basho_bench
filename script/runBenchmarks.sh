#!/bin/bash
set -u
set -e
AllNodes=`cat script/allnodes`

#Params: nodes, cookie, num of dcs, num of nodes, if connect dcs, replication or not, branch
./script/parallel_command.sh "$AllNodes" "sudo service ntp stop"
./script/stablizeTime.sh &
Pid=$!

#./script/makeRel.sh opt_speculation
./script/changePartition.sh 2 
./script/changeReplication.sh false
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 2 0 1 

./script/changePartition.sh 4 
./script/changeReplication.sh false
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 1 

sudo kill $Pid
