#!/bin/bash

AllNodes=`cat ./script/allnodes`
First=`head -1 ./script/allnodes`
Folder=$1
rm $Folder/specula_out
./script/parallel_fetch.sh "$AllNodes" ./basho_bench/tests/current/ summary.csv $Folder
rm  $Folder/tmp
head -1 $Folder/summary.csv-$First > $Folder/specula_out
paste $Folder/summary.csv-$First $Folder/summary.csv-$First | awk -F ',' '{print $1",", $2",", ($3-$8)",", ($4-$9)",", ($5-$10)",", ($6-$11)}' > $Folder/tmp
./script/summaryThroughput.sh $Folder
