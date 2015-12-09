#!/bin/bash


AllNodes=$1
Cookie=$2	
./script/startNodes.sh "$AllNodes"

NodesList=($1)
Length=${#NodesList[@]}
First=("${NodesList[@]:0:1}")
Others=("${NodesList[@]:1:$((Length-1))}")
sudo ./script/joinNodesToRing.sh $First "$Others"
./script/waitRingsToFinish.sh $First
