#!/bin/bash

AllNodes=`cat script/allnodes`
./script/stablizeTime.sh &
Pid=$!
./script/changePartitions.sh 2
./script/runMultiDCBenchmark.sh "$AllNodes"  antidote 1 2 1 
./script/changePartitions.sh 4 
./script/runMultiDCBenchmark.sh "$AllNodes"  antidote 1 4 1 

sudo kill $Pid
