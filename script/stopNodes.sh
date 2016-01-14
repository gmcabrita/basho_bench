#!/bin/bash

if [ $# -eq 0 ]
then
	AllNodes=`cat script/allnodes`
else
	AllNodes=$1	
fi

echo "Stopping nodes:" $AllNodes

StopAndR="sudo antidote/rel/antidote/bin/antidote stop && sudo rm -rf antidote/rel/antidote/data/* && sudo rm -rf antidote/rel/antidote/log/*" 
./script/parallel_command.sh "$AllNodes" "$StopAndR" 
