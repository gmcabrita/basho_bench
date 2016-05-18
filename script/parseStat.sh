#!/bin/bash

AllNodes=$1
Folder=$2
FetchName=$3
Good=false

Results=`sudo ./localScripts/getStat.sh "$1" $FetchName`
echo "Result is " "$Results"
Results=`echo "$Results" | tr '\n' ' ' `
Results=`cut -d "[" -f 2 <<< "$Results"`
Results=`cut -d "]" -f 1 <<< "$Results"`
#Results=(${Results//,/ })
if [[ $Results == *"Eshell"* ]]
then
    echo "Wrong format, try again!"
    Good=false
else
    Good=true
fi

if [ $Good == false ];
then
Results=`sudo ./localScripts/getStat.sh "$1" $FetchName`
echo "Result is " "$Results"
Results=`echo "$Results" | tr '\n' ' ' `
Results=`cut -d "[" -f 2 <<< "$Results"`
Results=`cut -d "]" -f 1 <<< "$Results"`
fi

echo "$Results" >> $Folder/stat
