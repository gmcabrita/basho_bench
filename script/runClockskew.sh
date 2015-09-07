#!/bin/bash
set -u
set -e
AllNodes=`cat script/allnodes`

#Params: nodes, cookie, num of dcs, num of nodes, if connect dcs, replication or not, branch
./script/makeRel.sh no_hybr_opt_specula 
File="./antidote/rel/antidote/antidote.config"

./script/changePartition.sh 4
./script/changeReplication.sh true chain 3 3 

./script/parallel_command.sh "$AllNodes" "sudo /usr/sbin/ntpdate -b time.example.com"
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 
./script/parallel_command.sh "$AllNodes" "sudo /usr/sbin/ntpdate -b time.example.com"
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 

./script/parallel_command.sh "$AllNodes" "sudo /usr/sbin/ntpdate -b time.example.com"
sleep 60
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 
./script/parallel_command.sh "$AllNodes" "sudo /usr/sbin/ntpdate -b time.example.com"
sleep 60
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 

./script/parallel_command.sh "$AllNodes" "sudo /usr/sbin/ntpdate -b time.example.com"
sleep 120
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 
./script/parallel_command.sh "$AllNodes" "sudo /usr/sbin/ntpdate -b time.example.com"
sleep 120
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 

./script/parallel_command.sh "$AllNodes" "sudo /usr/sbin/ntpdate -b time.example.com"
sleep 240
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 
./script/parallel_command.sh "$AllNodes" "sudo /usr/sbin/ntpdate -b time.example.com"
sleep 240
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 

./script/parallel_command.sh "$AllNodes" "sudo /usr/sbin/ntpdate -b time.example.com"
sleep 480
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 
./script/parallel_command.sh "$AllNodes" "sudo /usr/sbin/ntpdate -b time.example.com"
sleep 480
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 

./script/parallel_command.sh "$AllNodes" "sudo /usr/sbin/ntpdate -b time.example.com"
sleep 960 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 
./script/parallel_command.sh "$AllNodes" "sudo /usr/sbin/ntpdate -b time.example.com"
sleep 960 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 
