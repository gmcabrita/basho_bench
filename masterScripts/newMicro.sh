#!/bin/bash

function runNTimes {
    for i in $seq
    do
        if [ $start_ind -gt $skip_len ]; then
        ./script/runMicroBench.sh $t $MN $SN $CN $MR $SR $CR $do_specula $len $specula_read $rep $prob_access specula_tests $start_ind
        #echo $t $MN $SN $CN $MR $SR $CR $do_specula $len random $rep $comp specula_tests $start_ind
        skipped=1
        else
        echo "Skipped..."$start_index
        fi
        start_ind=$((start_ind+1))
    done
} 

#if [ $1 == true ]
#then
do_specula=true
fast_reply=true
#    length="8 4 2 1"
#else
#    do_specula=false
#    fast_reply=false
#    length="0"
#fi
## Just to test.. 
seq="1"
t="8"
#workloads="1 2 3 5"
contentions="1 2 3 4"
length="1 2 4 8 16"
localities="1"
local_comp="0"
prob_access=f
replications="2"
start_ind=1
skipped=1
skip_len=0
parts=3
BIG=10000
SML=1000
SR=10000
specula_read=specula
rep=1
for len in $length
do
    if [  $skipped == 1 ]
    then
    sudo ./script/configBeforeRestart.sh $t $do_specula $fast_reply $len $rep $parts $specula_read
    sudo ./script/restartAndConnect.sh
    sleep 25
    fi
    for cont in $contentions
    do
        if [ $cont == 1 ]; then MR=$BIG CR=$BIG
        elif [ $cont == 2 ]; then MR=$SML CR=$BIG
        elif [ $cont == 3 ]; then  MR=$SML CR=$SML
        elif [ $cont == 4 ]; then  MR=$SML CR=$SML
        fi
        MN=12    SN=0    CN=3
        if [ $skip_len == 0 ] || [ $skipped == 1 ]
        then
        sudo ./script/preciseTime.sh
        fi
        runNTimes
    done
done

specula_read=no_specula
for len in $length
do
    if [ $skip_len == 0 ] || [ $skipped == 1 ]
    then
    sudo ./script/configBeforeRestart.sh $t $do_specula $fast_reply $len $rep $parts $specula_read
    sudo ./script/restartAndConnect.sh
    sleep 25
    fi
    for cont in $contentions
    do
        if [ $cont == 1 ]; then MR=$BIG CR=$BIG
        elif [ $cont == 2 ]; then MR=$SML CR=$BIG
        elif [ $cont == 3 ]; then  MR=$SML CR=$SML
        elif [ $cont == 4 ]; then  MR=$SML CR=$SML
        fi
        MN=12    SN=0    CN=3
        if [ $skip_len == 0 ] || [ $skipped == 1 ]
        then
        sudo ./script/preciseTime.sh
        fi
        runNTimes
    done
done

specula_read=no_specula
do_specula=false
fast_reply=false
sudo ./masterScripts/initMachnines.sh 1 benchmark_no_specula
sudo ./masterScripts/initMachnines.sh 1 benchmark_no_specula
len=0
if [ $skip_len == 0 ] || [ $skipped == 1 ]
  then
  sudo ./script/configBeforeRestart.sh $t $do_specula $fast_reply $len $rep $parts $specula_read
  sudo ./script/restartAndConnect.sh
  sleep 25
fi
for cont in $contentions
do
    if [ $cont == 1 ]; then MR=$BIG CR=$BIG
    elif [ $cont == 2 ]; then MR=$SML CR=$BIG
    elif [ $cont == 3 ]; then  MR=$SML CR=$SML
    elif [ $cont == 4 ]; then  MR=$SML CR=$SML
    fi
    MN=12    SN=0    CN=3
    if [ $skip_len == 0 ] || [ $skipped == 1 ]
    then
    sudo ./script/preciseTime.sh
    fi
    runNTimes
done


spcula_read=specula
prob_access="t"
locals="1 2 3"
for len in $length
do
    if [ $skip_len == 0 ] || [ $skipped == 1 ]
    then
    sudo ./script/configBeforeRestart.sh $t $do_specula $fast_reply $len $rep $parts $specula_read
    sudo ./script/restartAndConnect.sh
    sleep 25
    fi
    MR=$BIG CR=$BIG
    for lo in $locals
    do
        if [ $lo == 1]; then MN=80 SN=19 CN=1
        elif [ $lo == 2 ]; then MN=70 SN=15 CN=10
        elif [ $lo == 3 ]; then  MN=50 SN=25 CN=25
        fi
        MN=12    SN=0    CN=3
        if [ $skip_len == 0 ] || [ $skipped == 1 ]
        then
        sudo ./script/preciseTime.sh
        fi
        runNTimes
    done
done

for len in $length
do
    if [ $skip_len == 0 ] || [ $skipped == 1 ]
    then
    sudo ./script/configBeforeRestart.sh $t false false 0 $rep $parts false 
    sudo ./script/restartAndConnect.sh
    sleep 25
    fi
    MR=$BIG CR=$BIG
    for lo in $locals
    do
        if [ $lo == 1]; then MN=80 SN=19 CN=1
        elif [ $lo == 2 ]; then MN=70 SN=15 CN=10
        elif [ $lo == 3 ]; then  MN=50 SN=25 CN=25
        fi
        MN=12    SN=0    CN=3
        if [ $skip_len == 0 ] || [ $skipped == 1 ]
        then
        sudo ./script/preciseTime.sh
        fi
        runNTimes
    done
done
