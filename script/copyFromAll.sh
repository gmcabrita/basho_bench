#!/bin/bash

FAIL=0
echo $command" for nodes:"$nodes 
if [ $# -eq 3 ]
then
    nodes=`cat ./script/allnodes`
    file=$1
    path=$2
    folder=$3
else
    nodes=$1
    file=$2
    path=$3
    folder=$4
fi
echo "Path is "$path
for node in $nodes
do
    FileName=${file/localhost/$node}
    SafeFileName=$node-$FileName
    scp -i key $file ubuntu@$node:$path/$FileName $folder/$SafeFileName & 
done

for job in `jobs -p`
do
    wait $job || let "FAIL+=1"
done
