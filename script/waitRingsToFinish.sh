#!/bin/bash

if [ $# -eq 0 ]
then
    Nodes=`head script/allnodes`
else
    Nodes=$1
fi

Status="sudo antidote/rel/antidote/bin/antidote-admin member-status"

for Node in $Nodes
do
while true; do
       LineNum=`./script/command_to_all.sh "$Node" "$Status" | grep "\-\-      'antidote" | wc -l`  
       if [ $LineNum -ne 0 ]; then
               echo "Ring joined for ${Node}!"
	       break
       else
               echo "Joining for ${Node}..."
       	       sleep 3 
       fi
done
done
