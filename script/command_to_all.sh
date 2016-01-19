#!/bin/bash
set -u
set -e
FAIL=0
if [ $# -eq 1 ]
then
    nodes=`cat ./script/allnodes`
    command="$1"
else
    nodes=$1
    command=$2
fi
echo $command" for nodes:"$nodes 
for node in $nodes
do
   ssh -o ConnectTimeout=3 -t ubuntu@$node -i key ${command/localhost/$node} 
done
echo $command done

for job in `jobs -p`
do
    wait $job || let "FAIL+=1"
done

if [ "$FAIL" == "0" ];
then
echo "$command finished." 
else
echo "Fail! ($FAIL)"
fi
