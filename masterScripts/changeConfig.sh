#!/bin/bash

if [ $# == 4 ]
then
Cmd="./basho_bench/localScripts/changeConfig.sh $2 $3 $4"  
else
Cmd="./basho_bench/localScripts/changeConfig.sh $2 $3 $4 $5"  
fi
echo "$Cmd"
./script/parallel_command.sh "$1" "$Cmd"
