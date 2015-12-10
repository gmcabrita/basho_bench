#!/bin/bash

./script/makeRel.sh local_specula_read
./script/makeRel.sh local_specula_read

./script/copy_to_all.sh ./script/allnodes ./basho_bench/script 
./script/command_to_all.sh "./basho_bench/masterScripts/config.sh" 
