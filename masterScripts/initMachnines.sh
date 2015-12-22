#!/bin/bash

if [ $# -eq 1 ]; then
    Clean=$1
else
    Clean=true
fi
./script/parallel_command.sh "cd basho_bench && git stash && git pull && sudo make"
./script/command_to_all.sh "./basho_bench/masterScripts/config.sh" 
./script/command_to_all.sh "cd ./basho_bench/ && sudo chown -R ubuntu specula_tests"
if [ "$Clean" == true ]
then
./script/makeRel.sh local_specula_read
./script/makeRel.sh local_specula_read
fi

./script/copy_to_all.sh ./script/allnodes ./basho_bench/script 
./script/command_to_all.sh "./basho_bench/masterScripts/config.sh" 
