#!/bin/bash

# Dist tune
seq="1"
#sudo ./masterScripts/initMachnines.sh 1 benchmark_precise_remove_stat_tune_read 
#sudo ./script/parallel_command.sh "cd antidote && sudo make rel"
sudo ./masterScripts/initMachnines.sh 1 benchmark_precise_remove_stat_forward
sudo ./script/parallel_command.sh "cd antidote && sudo make rel"
folder="specula_tests/precise"
clock=new
do_specula=false
specula_read=false
threads="40"
length="0"
len=0
num_keys="10 20 50 100 200"

sudo ./script/configBeforeRestart.sh 1000 $do_specula $len $rep $parts $specula_read
sudo ./script/restartAndConnect.sh

for t in $threads
do
    sudo ./script/configBeforeRestart.sh $t $do_specula $len $rep $parts $specula_read
    for keys in $num_keys
    do
        rm -rf ./config
        echo micro duration 60 >> config
        echo micro auto_tune false >> config
        echo micro centralized false >> config
        echo micro all_nodes replace >> config
        echo micro tune_period 2 >> config
        echo micro tune_sleep 1 >> config
        echo micro total_key $keys >> config
        sudo ./script/copy_to_all.sh ./config ./basho_bench/
        sudo ./script/parallel_command.sh "cd basho_bench && sudo ./script/config_by_file.sh"
        MR=$((10*keys*keys))
        CR=$((10*keys*keys))
        echo $MR $CR
        runNTimes
    done
done
