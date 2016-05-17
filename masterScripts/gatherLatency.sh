#!/bin/bash

AllNodes=`cat ./script/allnodes`

./script/parallel_command.sh "cd ./basho_bench/ && ./masterScripts/measureLatency.sh"

I=0
for Node in $AllNodes
do
    File=$I"summary-"$Node
    scp -i key ubuntu@$Node:./basho_bench/$File ./latency/ &
    I=$((I+1))
done
