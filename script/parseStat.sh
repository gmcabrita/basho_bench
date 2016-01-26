#!/bin/bash

Folder=$2
AllNodes=$1
FetchName=$3
Results=`sudo ./localScripts/getStat.sh "$1" $FetchName`
echo "Result is " "$Results"
Results=`echo "$Results" | tr '\n' ' ' `
Results=`cut -d "{" -f 2 <<< "$Results"`
Results=`cut -d "}" -f 1 <<< "$Results"`
Results=(${Results//,/ })
echo "Result is $Results"
ReadAborted=${Results[0]}
ReadInvalid=${Results[1]}
SpeculaAborted=${Results[2]}
CascadeAborted=${Results[3]}
Committed=${Results[4]}
SpeculaCommitted=${Results[5]}
SpeculaReadTxn=${Results[6]}
PartSpeculaRead=${Results[7]}
RepSpeculaRead=${Results[8]}
RepTotalRead=${Results[9]}
CacheSpeculaRead=${Results[10]}
CacheTotalRead=${Results[11]}
AvgLocalCert=${Results[12]}
AvgSpeculaAbort=${Results[13]}
AvgSpeculaCommit=${Results[14]}
echo $AllNodes":ReadAborted" $ReadAborted" ReadInvalid" $ReadInvalid " SpeculaAborted" $SpeculaAborted " CascadeAborted" $CascadeAborted" Committed" $Committed" speculaCommitted" $SpeculaCommitted  PartSpeculaRead "$PartSpeculaRead" RepSpec "$RepSpeculaRead" RepTotal "$RepTotalRead" CacheSpec "$CacheSpeculaRead" CacheTotal "$CacheTotalRead "LocalCert "$AvgLocalCert" SpeculaAbort "$AvgSpeculaAbort" SpeculaCommit "$AvgSpeculaCommit" >> $Folder/stat
