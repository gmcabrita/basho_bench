#!/bin/bash

function runNTimes {
    for i in $seq
    do
        if [ $start_ind -gt $skip_len ]; then
        ./script/runMicroBench.sh $t $MN $SN $CN $MR $SR $CR $do_specula $len $specula_read $rep $prob_access specula_tests $start_ind
        #echo $t $MN $SN $CN $MR $SR $CR $do_specula $len random $rep $comp specula_tests $start_ind
        skipped=1
        else
        echo "Skipped..."$start_ind
        fi
        start_ind=$((start_ind+1))
    done
} 

#################################### Keep in mind to change the order of the last specula experiment...

do_specula=true
fast_reply=true
## Just to test.. 
seq="1"
t="8"
contentions="1 2 3 4"
length="1 2 4 8 16"
localities="1"
local_comp="0"
prob_access=f
replications="2"
start_ind=1
skipped=0
skip_len=5
parts=3
MBIG=10000
MSML=500
CBIG=2000
CSML=200
SR=10000
specula_read=specula
rep=1

if [ $skip_len -eq 0 ]; then
sudo ./masterScripts/initMachnines.sh 1 benchmark_precise_fast_repl
fi

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
        if [ $cont == 1 ]; then MR=$MBIG CR=$CBIG
        elif [ $cont == 2 ]; then MR=$MSML CR=$CBIG
        elif [ $cont == 3 ]; then  MR=$MBIG CR=$CSML
        elif [ $cont == 4 ]; then  MR=$MSML CR=$CSML
        fi
        MN=12    SN=0    CN=3
        if [ $skip_len == 0 ] || [ $skipped == 1 ]
        then
        sudo ./script/preciseTime.sh
        fi
        runNTimes
    done
done

#Same, but no specula read
specula_read=nospecula
if [ $skipped == 1 ]
then
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
        if [ $cont == 1 ]; then MR=$MBIG CR=$CBIG
        elif [ $cont == 2 ]; then MR=$MSML CR=$CBIG
        elif [ $cont == 3 ]; then  MR=$MBIG CR=$CSML
        elif [ $cont == 4 ]; then  MR=$MSML CR=$CSML
        fi
        MN=12    SN=0    CN=3
        if [ $skip_len == 0 ] || [ $skipped == 1 ]
        then
        sudo ./script/preciseTime.sh
        fi
        runNTimes
    done
done
fi

skipped=1
spcula_read=specula
do_specula=true
fast_reply=true
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
    MR=$MBIG CR=$CBIG
    for lo in $locals
    do
        if [ $lo == 1 ]; then MN=80 SN=19 CN=1
        elif [ $lo == 2 ]; then MN=70 SN=15 CN=10
        elif [ $lo == 3 ]; then  MN=50 SN=25 CN=25
        fi
        if [ $skip_len == 0 ] || [ $skipped == 1 ]
        then
        sudo ./script/preciseTime.sh
        fi
        runNTimes
    done
done


#No speculation
specula_read=nospecula
do_specula=false
fast_reply=false
len=0
sudo ./masterScripts/initMachnines.sh 1 benchmark_no_specula
sudo ./masterScripts/initMachnines.sh 1 benchmark_no_specula
if [ $skip_len == 0 ] || [ $skipped == 1 ]
  then
  sudo ./script/configBeforeRestart.sh $t $do_specula $fast_reply $len $rep $parts $specula_read
  sudo ./script/restartAndConnect.sh
  sleep 25
fi
for cont in $contentions
do
    if [ $cont == 1 ]; then MR=$MBIG CR=$CBIG
    elif [ $cont == 2 ]; then MR=$MSML CR=$CBIG
    elif [ $cont == 3 ]; then  MR=$MBIG CR=$CSML
    elif [ $cont == 4 ]; then  MR=$MSML CR=$CSML
    fi
    MN=12    SN=0    CN=3
    if [ $skip_len == 0 ] || [ $skipped == 1 ]
    then
    sudo ./script/preciseTime.sh
    fi
    runNTimes
done

spcula_read=nospecula
do_specula=false
fast_reply=false
len=0
prob_access="t"
locals="1 2 3"
if [ $skip_len == 0 ] || [ $skipped == 1 ]
then
    sudo ./script/configBeforeRestart.sh $t $do_specula $fast_reply $len $rep $parts $specula_read 
    sudo ./script/restartAndConnect.sh
sleep 25
fi
MR=$MBIG CR=$CBIG
for lo in $locals
do
    if [ $lo == 1 ]; then MN=80 SN=19 CN=1
    elif [ $lo == 2 ]; then MN=70 SN=15 CN=10
    elif [ $lo == 3 ]; then  MN=50 SN=25 CN=25
    fi
    if [ $skip_len == 0 ] || [ $skipped == 1 ]
    then
        sudo ./script/preciseTime.sh
    fi
    runNTimes
done

#Still need scalability test
