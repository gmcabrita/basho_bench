#!/bin/bash

AllNodes=`cat ./script/allnodes`

for Node in $AllNodes
do
    ./script/parallel_command.sh "./basho_bench/masterScript/measureLatency.sh"
done

I=0
for Node in $AllNodes
do
    File=$I"summary"
    scp -i key ubuntu@$Node:./basho_bench/$File ./basho_bench/stat/ &
    I=$((I+1))
done
