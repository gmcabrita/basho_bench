#!/bin/bash

if [ $# -eq 0 ]; then
	AllNodes=`cat script/allnodes`
	Cookie="antidote"
	./script/stopNodes.sh "$AllNodes" 
	./script/startNodes.sh "$AllNodes"
elif [ $# -eq 1 ]; then
	AllNodes=`cat script/allnodes`
	Cookie="antidote"
	echo "Truning on CGroups is "$1
	./script/stopNodes.sh "$AllNodes" 
	./script/cgroupStartNodes.sh  $1
else
    AllNodes=$1
    Cookie=$2
    ./script/stopNodes.sh "$AllNodes" 
    ./script/startNodes.sh "$AllNodes"
fi

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
