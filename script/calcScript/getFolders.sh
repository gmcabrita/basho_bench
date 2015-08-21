#!/bin/bash

Pattern="$PWD/tests/*"
if [ "$1" -gt "$2" ]; then
    Start=$(($1+1))
    NumOfFile=$(($1-$2+1))
    FileName=`ls -d $Pattern | tail -$Start | head -$NumOfFile` 
    echo $FileName
else
    Start=$1
    NumOfFile=$(($2-$1+1))
    FileName=`ls -d $Pattern | head -$2 | tail -$NumOfFile` 
    echo $FileName
fi
