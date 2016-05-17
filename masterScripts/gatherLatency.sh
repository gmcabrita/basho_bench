#!/bin/bash

AllNodes=`cat ./script/allnodes`

for Node in $AllNodes
do
    ./script/parallel_command.sh "cd ./basho_bench/ && ./masterScripts/measureLatency.sh"
done

I=0
for Node in $AllNodes
do
    File=$I"summary"
    scp -i key ubuntu@$Node:./basho_bench/$File ./basho_bench/latency/ &
    I=$((I+1))
done
