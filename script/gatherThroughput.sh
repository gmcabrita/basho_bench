#!/bin/bash

AllNodes=`cat ./script/allnodes`
First=`head -1 ./script/allnodes`
Folder=$1
rm $Folder/*
./script/parallel_fetch.sh "$AllNodes" ./basho_bench/tests/current/summary.csv $Folder
paste $Folder/$First $Folder/$First | awk -F ',' '{print $1",", $2",", ($3-$7)",", ($4-$8)",", ($5-$9)" "}' > $1/specula_out
Files=`ls $Folder/1*`
for F in $Files
do
paste $1/specula_out $F | awk -F ',' '{print $1",",$2",",($3+$7)",",($4+$8)",",($5+$9)" "}' > $1/specula_out 
done
