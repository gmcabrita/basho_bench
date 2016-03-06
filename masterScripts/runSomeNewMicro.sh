#!/bin/bash

function runNTimes {
    for i in $seq
    do
        if [[ ${array[$start_ind]} ]]; then
            if [  $skipped -eq 0 ]
            then
            sudo ./script/configBeforeRestart.sh $t $do_specula $fast_reply $len $rep $parts $specula_read
            sudo ./script/restartAndConnect.sh
            sleep 25
            fi
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
skip_len=0
parts=28
rep=5
MBIG=20000
MSML=1000
CBIG=40000
CSML=2000
SR=100000
specula_read=specula

declare -A array
for num in $1
do
    array[$num]=1
    echo $num
done

prob_access=t
deter=false

###########

#sudo ./masterScripts/initMachnines.sh 1 benchmark_precise_fast_repl

MN=80    
SN=20    
CN=0
for len in $length
do
    if [  $skipped -eq 1 ]
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
        if [ $skipped -eq 1 ]
        then
        sudo ./script/preciseTime.sh
        fi
        runNTimes
    done
done

######40#######

#Same, but no specula read
MN=80    
SN=20    
CN=0
specula_read=nospecula
for len in $length
do
    if [ $skipped -eq 1 ]
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
        if  [ $skipped -eq 1 ]
        then
        sudo ./script/preciseTime.sh
        fi
        runNTimes
    done
done
exit
######80#######

######Rerun
#Test remote read
specula_read=specula
do_specula=true
fast_reply=true
prob_access=t
locals="1 2 3"
for len in $length
do
    if [ $skip_len -eq 0 ] || [ $skipped -eq 1 ]
    then
    sudo ./script/configBeforeRestart.sh $t $do_specula $fast_reply $len $rep $parts $specula_read
    sudo ./script/restartAndConnect.sh
    sleep 25
    fi
    MR=$MBIG CR=$CBIG
    for lo in $locals
    do
        if [ $lo == 1 ]; then MN=80 SN=19 CN=1
        elif [ $lo == 2 ]; then MN=80 SN=15 CN=5
        elif [ $lo == 3 ]; then  MN=80 SN=10 CN=10
        fi
        if [ $skip_len -eq 0 ] || [ $skipped -eq 1 ]
        then
        sudo ./script/preciseTime.sh
        fi
        runNTimes
    done
done
######110#######


#No speculation
MN=80    
SN=20    
CN=0
specula_read=nospecula
do_specula=false
fast_reply=false
deter=false
len=0
if [ $skip_len -eq 1 ]; then
sudo ./masterScripts/initMachnines.sh 1 benchmark_no_specula
sudo ./masterScripts/initMachnines.sh 1 benchmark_no_specula
fi


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
    if [ $skip_len == 0 ] || [ $skipped == 1 ]
    then
    sudo ./script/preciseTime.sh
    fi
    runNTimes
done
######158#######


specula_read=nospecula
do_specula=false
fast_reply=false
len=0
prob_access=t
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
    elif [ $lo == 2 ]; then MN=80 SN=15 CN=5
    elif [ $lo == 3 ]; then  MN=80 SN=10 CN=10
    fi
    if [ $skip_len == 0 ] || [ $skipped == 1 ]
    then
        sudo ./script/preciseTime.sh
    fi
    runNTimes
done
######164#######

#sudo ./masterScripts/initMachnines.sh 1 benchmark_precise_fast_repl
#Test number of involved DCs
skipped=0
specula_read=specula
do_specula=true
fast_reply=true
deters="1 2 3 4 5"
MN=20 SN=80 CN=0
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
