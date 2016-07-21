#!/bin/bash

AllNodes=`cat script/allnodes` 
echo "Starting nodes:" $AllNodes
if [ $# -eq 0 ]
then
Start="sudo antidote/rel/antidote/bin/antidote start"
./script/parallel_command.sh "$AllNodes" "$Start" 
else
    if "$1" ;
    then
    echo "###################Starting in cgroup!!"
    Start="sudo cgexec -g cpu:antidote antidote/rel/antidote/bin/antidote start"
    ./script/parallel_command.sh "$AllNodes" "$Start" 
    else
    Start="sudo antidote/rel/antidote/bin/antidote start"
    ./script/parallel_command.sh "$AllNodes" "$Start" 
    fi
fi


