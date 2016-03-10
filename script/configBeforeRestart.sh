#!/bin/bash

sudo rm ./config
repl_degree=$5
echo ant concurrent $1 >> ./config
echo ant do_specula $2  >> ./config
echo ant fast_reply $3   >> ./config
echo ant specula_length $4  >> ./config
echo ant specula_read $7 >> ./config
echo app_config ring_creation_size $6 >> ./config
echo ant num_dcs  `cat ./script/num_dcs` >> ./config
echo ant do_repl true >> ./config

sudo ./script/copy_to_all.sh ./config ./basho_bench/
sudo ./script/parallel_command.sh "cd basho_bench && sudo ./script/config_by_file.sh && sudo ./script/configReplication.sh $repl_degree"
