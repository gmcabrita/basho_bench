#!/bin/bash

AllNodes=`cat ./script/allnodes`
First=`head -1 $AllNodes`
mkdir -p $Result
Folder=tmp
./script/parallel_fetch.sh "$AllNodes" ./basho_bench/tests/current/summary.csv $Folder
paste <<< ls $Folder/* | awk -F ',' '{print $1, $2, ($3+$7), ($4+$8), ($5+$9)}' > $1/specula_out 
