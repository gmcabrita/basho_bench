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
ReadAborted=${Results[0]}
SpeculaAborted=${Results[1]}
Committed=${Results[2]}
SpeculaCommitted=${Results[3]}
SpeculaReadTxn=${Results[4]}
PartSpeculaRead=${Results[5]}
RepSpeculaRead=${Results[6]}
RepTotalRead=${Results[7]}
CacheSpeculaRead=${Results[8]}
CacheTotalRead=${Results[9]}
echo "ReadAborted" $ReadAborted " SpeculaAborted" $SpeculaAborted" Committed"$Committed" speculaCommitted"$SpeculaCommitted  PartSpeculaRead "$PartSpeculaRead" RepSpec "$RepSpeculaRead" RepTotal "$RepTotalRead" CacheSpec "$CacheSpeculaRead" CacheTotal "$CacheTotalRead" >> $Folder/stat
