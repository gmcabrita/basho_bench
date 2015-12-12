#!/bin/bash

if [ $# -eq 0 ]
then
	AllNodes=`cat script/allnodes` 
else
	AllNodes=$1	
fi

echo "Starting nodes:" $AllNodes

Start="sudo antidote/rel/antidote/bin/antidote start"
./script/parallel_command.sh "$AllNodes" "$Start" 
