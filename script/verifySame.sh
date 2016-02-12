#!/bin/bash
set -e
Folder=$1
TotalTried=`cat $Folder/specula_out | awk -F ',' '{print $3}' | awk '{S+=$1}END{print S}'`
TotalStat=`cat $Folder/stat | awk -F ' ' '{print $2" " $4" " $6 " " $8" " $10}' | awk '{t=0; for(i=1;i<=NF;i++) t+=$i; print t}' | awk '{t+=$i}END{print t}'`
Diff=$((TotalTried-TotalStat))
echo $TotalTried $TotalStat >> $Folder/diff
if [ $Diff -lt 10 ];
then
    echo "Almost ok, tried "$TotalTried", stat is "$TotalStat
else
    echo "Something is wrong!!!!"$TotalTried ", "$TotalStat
fi
