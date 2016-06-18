#!/bin/bash

sudo rm ./config
echo ant concurrent $1 >> ./config
echo ant do_specula $2  >> ./config
echo ant specula_length $3  >> ./config
echo ant specula_read $6 >> ./config
repl_degree=$4
echo app_config ring_creation_size $5 >> ./config
echo ant num_dcs  `cat ./script/num_dcs` >> ./config
echo ant do_repl true >> ./config

sudo ./script/parallel_command.sh "cd basho_bench && sudo chown -R ubuntu ./config"
sudo ./script/copy_to_all.sh ./config ./basho_bench/
sudo ./script/parallel_command.sh "cd basho_bench && sudo ./script/config_by_file.sh && sudo ./script/configReplication.sh $repl_degree"
