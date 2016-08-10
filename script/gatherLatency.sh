#!/bin/bash

AllNodes=`cat ./script/allnodes`
First=`head -1 ./script/allnodes`
Folder=$1
./script/parallel_fetch.sh "$AllNodes" ./basho_bench/tests/current/percv_latency $Folder
./script/parallel_fetch.sh "$AllNodes" ./basho_bench/tests/current/final_latency $Folder
#### Take latency from [30, 60) ####
### Assume that the counter goes every 5 seconds, so my range should be Number 7 to Number 13 
for i in `ls $Folder/*percv_latency*`;
do
    echo $i | sed '1,/^Number 6$/d' | sed '/Number 12/q' >> summary_percv
done

for i in `ls $Folder/*final_latency*`;
do
    echo $i | sed '1,/^Number 6$/d' | sed '/Number 12/q' >> summary_final 
done
