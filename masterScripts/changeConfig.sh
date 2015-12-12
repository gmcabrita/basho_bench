#!/bin/bash

Cmd="./basho_bench/localScripts/changeConfig.sh $2 $3 $4"  
echo "$Cmd"
./script/parallel_command.sh "$1" "$Cmd"
