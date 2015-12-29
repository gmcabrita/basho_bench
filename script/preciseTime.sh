#!/bin/bash

AllNodes=`cat script/allnodes`
FirstNode=`head -1 script/allnodes`
Command3="sudo service ntp stop"
Command4="sudo /usr/sbin/ntpdate -b ntp.ubuntu.com"
./script/parallel_command.sh "$AllNodes" "$Command3"	
./script/parallel_command.sh "$AllNodes" "$Command4"	
