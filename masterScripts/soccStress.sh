#!/bin/bash
set -e

function runNTimes {
    for i in $seq
    do
        if [ $start_ind -gt $skip_len ]; then
        sudo ./script/preciseTime.sh
        ./script/runMicroBench.sh $t $MN $SN $CN $MR $SR $CR $do_specula $len $specula_read $rep $prob_access $deter specula_tests $start_ind $clock 
        skipped=1
        else
        echo "Skipped..."$start_ind
        fi
        start_ind=$((start_ind+1))
    done
} 

do_specula=true
fast_reply=true
seq="1"
threads="16 24 32 40 48 56 64"
#threads="4 128 180"
t=8
contentions="2"
length="4"
start_ind=1
skipped=1
skip_len=0
rep=2
parts=4
#rep=5
#parts=28
MBIG=20000
MSML=1000
CBIG=40000
CSML=2000
MR=$MBIG 
CR=$CBIG
SR=100000
specula_read=true

prob_access=t
deter=false

#Test remote read
MN=80
SN=20
CN=0

sudo ./masterScripts/initMachnines.sh 1 benchmark_precise_fast_repl
sudo ./script/parallel_command.sh "cd antidote && sudo make rel"

clock="new"
specula_read=true
do_specula=true
fast_reply=true
prob_access=t

rm -rf ./config
echo micro cdf true >> config
echo micro duration 90 >> config
echo ant cdf true >> ./config
sudo ./script/copy_to_all.sh ./config ./basho_bench/
sudo ./script/parallel_command.sh "cd basho_bench && sudo ./script/config_by_file.sh"

sudo ./script/configBeforeRestart.sh $t $do_specula $fast_reply 8 $rep $parts $specula_read
sudo ./script/restartAndConnect.sh

for t in $threads
do
for len in $length
do
    sudo ./script/configBeforeRestart.sh $t $do_specula $fast_reply $len $rep $parts $specula_read
    for cont in $contentions
    do
        if [ $cont == 1 ]; then MR=$MBIG CR=$CBIG
        elif [ $cont == 2 ]; then MR=$MSML CR=$CBIG
        elif [ $cont == 3 ]; then  MR=$MBIG CR=$CSML
        elif [ $cont == 4 ]; then  MR=$MSML CR=$CSML
        fi
        runNTimes
    done
done
done
exit

######100#######

sudo ./masterScripts/initMachnines.sh 1 benchmark_no_specula
sudo ./script/parallel_command.sh "cd antidote && sudo make rel"

rm -rf ./config
echo micro cdf true >> config
echo ant cdf true >> ./config
echo micro duration 90 >> config
sudo ./script/copy_to_all.sh ./config ./basho_bench/
sudo ./script/parallel_command.sh "cd basho_bench && sudo ./script/config_by_file.sh"

clock="old"
specula_read=false
do_specula=false
fast_reply=false
len=0
prob_access=t
sudo ./script/configBeforeRestart.sh $t $do_specula $fast_reply $len $rep $parts $specula_read 
sudo ./script/restartAndConnect.sh
for t in $threads
do
for cont in $contentions
do
    if [ $cont == 1 ]; then MR=$MBIG CR=$CBIG
    elif [ $cont == 2 ]; then MR=$MSML CR=$CBIG
    elif [ $cont == 3 ]; then  MR=$MBIG CR=$CSML
    elif [ $cont == 4 ]; then  MR=$MSML CR=$CSML
    fi
    runNTimes
done
done
######194#######
