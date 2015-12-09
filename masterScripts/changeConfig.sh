#!/bin/bash

Cmd="./basho_bench/localScripts/changeConfig.sh $2 $3 $4" 
./script/command_to_all.sh "$1" "$Cmd"
