#!/bin/bash

function runNTimes {
    for i in $seq
    do
        if [ $start_ind -gt $skip_len ]; then
        ./script/runMicroBench.sh $t $MN $SN $CN $MR $SR $CR $do_specula $len $specula_read $rep $prob_access $deter specula_tests $start_ind
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
length="1 2 4 8 16"
start_ind=1
skipped=0
skip_len=124
parts=4
rep=1

#Test number of involved DCs
specula_read=specula
do_specula=true
fast_reply=true
deters="1 2 3 4 5"
MN=20 SN=80 CN=0
skipped=0
MBIG=40000
CBIG=60000
for len in $length
do
    if [ $skip_len == 0 ] || [ $skipped == 1 ]
    then
    sudo ./script/configBeforeRestart.sh $t $do_specula $fast_reply $len $rep $parts $specula_read
    sudo ./script/restartAndConnect.sh
    sleep 25
    fi
    MR=$MBIG CR=$CBIG
    for deter in $deters
    do
        if [ $skip_len == 0 ] || [ $skipped == 1 ]
        then
        sudo ./script/preciseTime.sh
        fi
        runNTimes
    done
done
######150#######

#Test number of involved DCs
sudo ./masterScripts/initMachnines.sh 1 benchmark_no_specula
prob_access=t
locals="1 2 3"
specula_read=nospecula
do_specula=false
fast_reply=false
deters="1 2 3 4 5"
MN=20 SN=80 CN=0
len=0
if [ $skip_len == 0 ] || [ $skipped == 1 ]
then
    sudo ./script/configBeforeRestart.sh $t $do_specula $fast_reply $len $rep $parts $specula_read
    sudo ./script/restartAndConnect.sh
    sleep 25
fi
MR=$MBIG CR=$CBIG
for deter in $deters
do
    if [ $skip_len == 0 ] || [ $skipped == 1 ]
    then
    sudo ./script/preciseTime.sh
    fi
    runNTimes
done
######174#######
