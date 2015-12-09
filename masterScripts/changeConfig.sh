#!/bin/bash

Cmd="./basho_bench/localScripts/changeConfig.sh $2 $3 $4"  
echo "$Cmd"
./script/command_to_all.sh "$1" "$Cmd"
