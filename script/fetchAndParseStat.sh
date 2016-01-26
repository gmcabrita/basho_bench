#!/bin/bash

AllNodes=`cat ./script/allnodes`
Folder=$1
./script/parallel_command.sh "cd basho_bench && sudo rm -f ./stat && sudo ./script/parseStat.sh localhost ."
./script/copyFromAll.sh stat ./basho_bench/ $Folder                 

for N in $AllNodes
do
cat $Folder/$N-stat >> $Folder/stat
rm $Folder/$N-stat
done

