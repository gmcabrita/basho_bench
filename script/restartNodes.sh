#!/bin/bash

if [ $# -eq 0 ]
then
	AllNodes=`cat script/allnodes`
else
	AllNodes=$1	
fi

echo $AllNodes

RestartNodes="sudo antidote/rel/antidote/bin/antidote stop && sudo rm -rf antidote/rel/antidote/log/* && sudo antidote/rel/antidote/bin/antidote start"
./script/parallel_command.sh "$AllNodes" "$RestartNodes"

