#!/bin/bash

Folder=$2
AllNodes=$1
FetchName=$3
Results=`sudo ./localScripts/getStat.sh "$1" $FetchName`
echo "Result is " "$Results"
Results=`echo "$Results" | tr '\n' ' ' `
Results=`cut -d "[" -f 2 <<< "$Results"`
Results=`cut -d "]" -f 1 <<< "$Results"`
Results=(${Results//,/ })
echo "Result is $Results"
Header="ReadAborted ReadInvalid CertAborted CascadeAborted Committed Whatever SpeculaRead Whatever NOCommitLP NOCommitRP NOAbortLP NOAbortRP PCommitLP PCommitRP PAbortLP PAbortRP GCommitLP GCommitRP GAbortLP GAbortRP"
echo "$Header" >> $Folder/stat
echo "$Results" >> $Folder/stat
