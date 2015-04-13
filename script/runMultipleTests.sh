#!/bin/bash

AllNodes=`cat script/allnodes`
#Mode="pb"
./script/preciseTime.sh
./script/runMultiDCBenchmark.sh "$AllNodes"  antidote 3 2 1 "rep1" 
./script/runMultiDCBenchmark.sh "$AllNodes"  antidote 3 2 1 "rep2" 
./script/runMultiDCBenchmark.sh "$AllNodes"  antidote 3 2 1 "rep3" 
