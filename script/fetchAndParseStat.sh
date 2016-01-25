#!/bin/bash
AllNodes=`cat ./script/allnodes`
Folder=$1

index=0
for N in $AllNodes
do
echo $index
./script/parseStat.sh $N $Folder $index 
index=$((index+1))
done

wait
for N in $AllNodes
do
cat $Folder/$N-stat >> $Folder/stat
rm $Folder/$N-stat
done
