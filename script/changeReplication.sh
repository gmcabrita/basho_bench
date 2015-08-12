#!/bin/bash

AllNodes=`cat script/allnodes`
File="./antidote/rel/antidote/antidote.config"

./script/replaceConfig.sh do_repl $1 $File
if [ $1 == true ] 
then
    ./script/replaceConfig.sh repl_factor $2 $File
    ./script/replaceConfig.sh quorum $3 $File
fi
