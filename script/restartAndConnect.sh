#!/bin/bash

if [ $# -eq 0 ]; then
	echo "Usage: all_nodes, cookie, number_of_dcs, nodes_per_dc"
	exit
else
    AllNodes=$1
    Cookie=$2
	./script/stopNodes.sh "$AllSystemNodes" 
	./script/startNodes.sh "$AllNodes"

	NodesList=($1)
	Length=${#NodesList[@]}
	First=("${NodesList[@]:0:1}")
	Others=("${NodesList[@]:1:$((Length-1))}")
	sudo ./script/joinNodesToRing.sh $First "$Others"
	./script/waitRingsToFinish.sh $First
fi

