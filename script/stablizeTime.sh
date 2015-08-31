#!/bin/bash

AllNodes=`cat script/allnodes`
while [ 1 == 1 ] ;
do
    Command4="sudo /usr/sbin/ntpdate -b time.example.com"
    ./script/parallel_command.sh "$AllNodes" "$Command4"
    sleep $1 
done
