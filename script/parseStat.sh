#!/bin/bash

AllNodes=$1
Folder=$2
FetchName=$3
Good=false

for i in `seq 1 100`
do
Results=`sudo ./localScripts/getStat.sh "$1" $FetchName`
echo "Result is " "$Results"
Results=`echo "$Results" | tr '\n' ' ' `
Results=`cut -d "[" -f 2 <<< "$Results"`
Results=`cut -d "]" -f 1 <<< "$Results"`
if [[ $Results == *"Eshell"* ]]
then
    echo "Wrong format, try again!"
else
    break
fi
done

if [ $Good == false ];
then
Results=`sudo ./localScripts/getStat.sh "$1" $FetchName`
echo "Result is " "$Results"
Results=`echo "$Results" | tr '\n' ' ' `
Results=`cut -d "[" -f 2 <<< "$Results"`
Results=`cut -d "]" -f 1 <<< "$Results"`
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
