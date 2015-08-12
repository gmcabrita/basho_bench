#!/bin/bash

AllNodes=`cat script/allnodes`
Command="sudo sed -i  's/{$1.*/{$1, $2}./' $3"
./script/parallel_command.sh "$AllNodes" "$Command"
