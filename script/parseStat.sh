#!/bin/bash

Folder=$1
AllNodes=`cat ./script/allnodes`
Results=`sudo ./localScripts/getStat.sh "$AllNodes"`
echo "Result is " "$Results"
Results=`echo "$Results" | tail -1`
Results=`cut -d ">" -f 2 <<< "$Results"`
Results=`cut -d "(" -f 1 <<< "$Results"`
Results=(${Results//,/ })
echo "Result is $Results"
HitCache=${Results[0]}
ReadAborted=${Results[1]}
SpeculaAborted=${Results[2]}
Committed=${Results[3]}
SpeculaCommitted=${Results[4]}
SpeculaReadTxn=${Results[5]}
PartSpeculaRead=${Results[6]}
RepSpeculaRead=${Results[7]}
RepTotalRead=${Results[8]}
CacheSpeculaRead=${Results[9]}
CacheTotalRead=${Results[10]}
echo "Hit cache:" $HitCache ", ReadAborted:" $ReadAborted ", SpeculaAborted:" $SpeculaAborted", Committed:"$Committed", speculaCommitted:"$SpeculaCommitted  Part "$PartSpeculaRead" RepSpec "$RepSpeculaRead" RepTotal "$RepTotalRead" CacheSpec "$CacheSpeculaRead" CacheTotal "$CacheTotalRead" >> $Folder/stat
