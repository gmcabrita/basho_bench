#!/bin/bash

AllNodes=`cat script/allnodes`
File="./antidote/rel/antidote/antidote.config"

./script/replaceConfig.sh do_repl $1 $File
if [ $1 == true ] 
then
    ./script/replaceConfig.sh mode $2 $File
    ./script/replaceConfig.sh repl_factor $3 $File
    ./script/replaceConfig.sh quorum $4 $File
fi
