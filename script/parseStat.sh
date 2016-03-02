#!/bin/bash

Folder=$2
AllNodes=$1
FetchName=$3
Good=false

while [ $Good == false ]
do
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
done

echo "$Results" >> $Folder/stat
