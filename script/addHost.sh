#!/bin/bash

AllNodes=`cat script/allnodes`
IP=`hostname -I`
Command0="sed -i '/example/d' /etc/hosts" 
Command1="echo $IP time.example.com | sudo tee --append /etc/hosts"
./script/command_to_all.sh "$AllNodes" "$Command0"	
./script/command_to_all.sh "$AllNodes" "$Command1"	
