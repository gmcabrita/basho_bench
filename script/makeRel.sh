#!/bin/bash

AllNodes=`cat script/allnodes`
File1="./antidote/rel/vars.config"
File2="./antidote/rel/files/app.config"
if [ $# -eq 0 ]
then
    Command1="sudo ./antidote/rel/antidote/bin/antidote stop" 
    Command2="cd ./antidote/ && sudo make rel"
    ./script/parallel_command.sh "$AllNodes" "$Command1"	
    ./script/parallel_command.sh "$AllNodes" "$Command2"	
else
    Branch=$1
    echo "Switching to branch $1 and remake!"
    Command1="sudo ./antidote/rel/antidote/bin/antidote stop" 
    Command2="cd ./antidote/ && sudo git reset --hard && sudo git fetch && sudo git checkout $Branch && sudo git pull"
    Command3="sudo sed -i 's/127.0.0.1/localhost/g' $File1"
    Command4="sudo sed -i 's/172.31.30.71/localhost/g' $File1"
    Command5="sudo sed -i 's/127.0.0.1/localhost/g' $File2"
    Command6="cd ./antidote/ && sudo make rel"
    ./script/parallel_command.sh "$AllNodes" "$Command1"	
    ./script/parallel_command.sh "$AllNodes" "$Command2"	
    ./script/parallel_command.sh "$AllNodes" "$Command3"	
    ./script/parallel_command.sh "$AllNodes" "$Command4"	
    ./script/parallel_command.sh "$AllNodes" "$Command5"	
    ./script/parallel_command.sh "$AllNodes" "$Command6"	
fi


