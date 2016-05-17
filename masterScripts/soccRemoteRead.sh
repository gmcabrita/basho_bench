#!/bin/bash

function runNTimes {
    for i in $seq
    do
        if [ $start_ind -gt $skip_len ]; then
        ./script/runMicroBench.sh $t $MN $SN $CN $MR $SR $CR $do_specula $len $specula_read $rep $prob_access $deter specula_tests $start_ind $clock 
        #echo $t $MN $SN $CN $MR $SR $CR $do_specula $len random $rep $comp specula_tests $start_ind
        skipped=1
        else
        echo "Skipped..."$start_ind
        fi
        start_ind=$((start_ind+1))
    done
} 

do_specula=true
fast_reply=true
## Just to test.. 
seq="1 2"
t="8"
contentions="1 2 3 4"
length="16 1 2 4 8"
start_ind=1
skipped=1
skip_len=0
rep=5
parts=28
MBIG=20000
MSML=1000
CBIG=40000
CSML=2000
MR=$MBIG 
CR=$CBIG
SR=100000
specula_read=specula

prob_access=t
deter=false

###########


#Test remote read
sudo ./masterScripts/initMachnines.sh 1 benchmark_precise_fast_repl
clock="new"
specula_read=specula
do_specula=true
fast_reply=true
prob_access=t
locals="1 2 3 4"
for len in $length
do
    if [ $skip_len -eq 0 ] || [ $skipped -eq 1 ]
    then
    sudo ./script/configBeforeRestart.sh $t $do_specula $fast_reply $len $rep $parts $specula_read
    sudo ./script/restartAndConnect.sh
    sleep 25
    fi
    for lo in $locals
    do
        if [ $lo == 1 ]; then MN=100 SN=0 CN=0
        elif [ $lo == 2 ]; then MN=95 SN=0 CN=5
        elif [ $lo == 3 ]; then  MN=80 SN=0 CN=20
        elif [ $lo == 4 ]; then  MN=50 SN=0 CN=50
        fi
        if [ $skip_len -eq 0 ] || [ $skipped -eq 1 ]
        then
        sudo ./script/preciseTime.sh
        fi
        runNTimes
    done
done
######100#######

sudo ./masterScripts/initMachnines.sh 1 benchmark_no_specula
clock="old"
specula_read=nospecula
do_specula=false
fast_reply=false
len=0
prob_access=t
locals="1 2 3 4"
if [ $skip_len == 0 ] || [ $skipped == 1 ]
then
    sudo ./script/configBeforeRestart.sh $t $do_specula $fast_reply $len $rep $parts $specula_read 
    sudo ./script/restartAndConnect.sh
    sleep 25
fi
for lo in $locals
do
    if [ $lo == 1 ]; then MN=100 SN=0 CN=0
    elif [ $lo == 2 ]; then MN=95 SN=0 CN=5
    elif [ $lo == 3 ]; then  MN=80 SN=0 CN=20
    elif [ $lo == 4 ]; then  MN=50 SN=0 CN=50
    fi
    if [ $skip_len == 0 ] || [ $skipped == 1 ]
    then
        sudo ./script/preciseTime.sh
    fi
    runNTimes
done
######194#######
