#!/bin/bash

Cmd="./localScripts/changeConfig.sh $2 $3 $4" 
./scripts/command_to_all.sh "$1" "$Cmd"
