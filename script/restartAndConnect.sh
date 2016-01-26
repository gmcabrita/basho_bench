#!/bin/bash

if [ $# -eq 0 ]; then
	AllNodes=`cat script/allnodes`
	Cookie="antidote"
else
    AllNodes=$1
    Cookie=$2
fi
	./script/stopNodes.sh "$AllNodes" 
	./script/startNodes.sh "$AllNodes"

	NodesList=($AllNodes)
	Length=${#NodesList[@]}
	First=("${NodesList[@]:0:1}")
	Others=("${NodesList[@]:1:$((Length-1))}")
	OtherList=`echo ${Others[@]}`
	sudo ./script/joinNodesToRing.sh $First "$OtherList"
	for Node in $AllNodes
	do
	./script/waitRingsToFinish.sh $Node & 
	done
	wait
