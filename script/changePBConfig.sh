#!/bin/bash

AllNodes=$1
File="./"$2
PerNodeNum=$3
Thread=0
BenchConfig="{antidote_pb_ips, ["
for Node in $AllNodes
do
    Node=\'$Node\',
    BenchConfig=$BenchConfig$Node
    Thread=$((Thread+PerNodeNum))
done
BenchConfig=${BenchConfig::-1}"]}."
echo $BenchConfig
sudo sed -i "s/{antidote_pb_ips.*/$BenchConfig/" $File
sudo sed -i "s/{concurrent,.*/{concurrent, $Thread}./" $File
