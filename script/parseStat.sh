#!/bin/bash

Folder=$2
AllNodes=$1
Results=`sudo ./localScripts/getStat.sh "$1"`
echo "Result is " "$Results"
Results=`echo "$Results" | tail -1`
Results=`cut -d "{" -f 2 <<< "$Results"`
Results=`cut -d "}" -f 1 <<< "$Results"`
Results=(${Results//,/ })
echo "Result is $Results"
ReadAborted=${Results[0]}
SpeculaAborted=${Results[1]}
CascadeAborted=${Results[2]}
Committed=${Results[3]}
SpeculaCommitted=${Results[4]}
SpeculaReadTxn=${Results[5]}
PartSpeculaRead=${Results[6]}
RepSpeculaRead=${Results[7]}
RepTotalRead=${Results[8]}
CacheSpeculaRead=${Results[9]}
CacheTotalRead=${Results[10]}
AvgSpeculaAbort=${Results[11]}
AvgSpeculaCommit=${Results[12]}
echo $AllNodes ":ReadAborted" $ReadAborted " SpeculaAborted" $SpeculaAborted " CascadeAborted" $CascadeAborted" Committed" $Committed" speculaCommitted" $SpeculaCommitted  PartSpeculaRead "$PartSpeculaRead" RepSpec "$RepSpeculaRead" RepTotal "$RepTotalRead" CacheSpec "$CacheSpeculaRead" CacheTotal "$CacheTotalRead" SpeculaAbort "$AvgSpeculaAbort" SpeculaCommit "$AvgSpeculaCommit" >> $Folder/stat
