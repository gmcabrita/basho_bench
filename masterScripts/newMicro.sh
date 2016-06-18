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
## Just to test.. 
seq="1 2"
t="8"
contentions="1 2 3 4"
length="16 1 2 4 8"
start_ind=1
skipped=0
skip_len=116
rep=5
parts=28
MBIG=20000
MSML=1000
CBIG=40000
CSML=2000
SR=100000
specula_read=true

prob_access=t
deter=false
clock="new"

###########

sudo ./masterScripts/initMachnines.sh 1 benchmark_no_specula
#sudo ./masterScripts/initMachnines.sh 1 benchmark_no_specula
#sudo ./masterScripts/initMachnines.sh 1 benchmark_precise_fast_repl

MN=80
SN=20
CN=0
###Just to test
specula_read=true
do_specula=true
locals="1 2 3"
#MBIG=40000
#CBIG=60000
for len in $length
do
    if [ $start_ind == 0 ] || [ $skipped == 1 ]
    then
    sudo ./script/configBeforeRestart.sh $t $do_specula $len $rep $parts $specula_read
    sudo ./script/restartAndConnect.sh
    sleep 25
    fi
    MR=$MBIG CR=$CBIG
    for lo in $locals
    do
        if [ $lo == 1 ]; then MN=70 SN=30 CN=0
        elif [ $lo == 2 ]; then MN=50 SN=50 CN=0
        elif [ $lo == 3 ]; then  MN=20 SN=80 CN=0
        fi
        if [ $skip_len == 0 ] || [ $skipped == 1 ]
        then
        sudo ./script/preciseTime.sh
        fi
        runNTimes
    done
done


MN=80 SN=20 CN=0
clock="old"
#sudo ./masterScripts/initMachnines.sh 1 benchmark_no_specula
#sudo ./masterScripts/initMachnines.sh 1 benchmark_no_specula
for len in $length
do
    if [  $skipped -eq 1 ] || [ $start_ind == 0 ]
    then
    sudo ./script/configBeforeRestart.sh $t $do_specula $len $rep $parts $specula_read
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
        if [ $skip_len -eq 0 ] || [ $skipped -eq 1 ]
        then
        sudo ./script/preciseTime.sh
        fi
        runNTimes
    done
done
#140

# Old clock and but no specula read
MN=80    
SN=20    
CN=0
specula_read=false
for len in $length
do
    if [ $skip_len -eq 0 ] || [ $skipped -eq 1 ]
    then
    sudo ./script/configBeforeRestart.sh $t $do_specula $len $rep $parts $specula_read
    sudo ./script/restartAndConnect.sh
    sleep 20
    fi
    for cont in $contentions
    do
        if [ $cont == 1 ]; then MR=$MBIG CR=$CBIG
        elif [ $cont == 2 ]; then MR=$MSML CR=$CBIG
        elif [ $cont == 3 ]; then  MR=$MBIG CR=$CSML
        elif [ $cont == 4 ]; then  MR=$MSML CR=$CSML
        fi
        if [ $skip_len -eq 0 ] || [ $skipped -eq 1 ]
        then
        sudo ./script/preciseTime.sh
        fi
        runNTimes
    done
done

prob_access=t
locals="1 2 3"
specula_read=false
do_specula=false
MN=20 SN=80 CN=0
len=0
MR=$MBIG CR=$CBIG
for lo in $locals
    do
    if [ $lo == 1 ]; then MN=70 SN=30 CN=0
    elif [ $lo == 2 ]; then MN=50 SN=50 CN=0
    elif [ $lo == 3 ]; then  MN=20 SN=80 CN=0
    fi
    if [ $skip_len == 0 ] || [ $skipped == 1 ]
    then
    sudo ./script/preciseTime.sh
    fi
    runNTimes
done

specula_read=false
do_specula=false
deter=false
len=0
threads="128 190"
MN=80 SN=20 CN=0

seq="1"
#sudo ./masterScripts/initMachnines.sh 1 benchmark_no_specula
#sudo ./script/configBeforeRestart.sh $t $do_specula $len $rep $parts $specula_read
#sudo ./script/restartAndConnect.sh
for t in $threads
do
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
done
exit
#####204#####



for len in $length
do
    if [  $skipped -eq 1 ]
    then
    sudo ./script/configBeforeRestart.sh $t $do_specula $len $rep $parts $specula_read
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
        if [ $skip_len -eq 0 ] || [ $skipped -eq 1 ]
        then
        sudo ./script/preciseTime.sh
        fi
        runNTimes
    done
done
#Finished 40

####### Test key locality
### 70

#Test remote read
specula_read=true
do_specula=true
prob_access=t
locals="1 2 3"
for len in $length
do
    if [ $skip_len -eq 0 ] || [ $skipped -eq 1 ]
    then
    sudo ./script/configBeforeRestart.sh $t $do_specula $len $rep $parts $specula_read
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
######100#######

#####
##### Same, but no old clock
#180

#No speculation
MN=80    
SN=20    
CN=0
specula_read=false
do_specula=false
deter=false
len=0
skipped=1

if [ $skip_len == 0 ] || [ $skipped == 1 ]
  then
  sudo ./script/configBeforeRestart.sh $t $do_specula $len $rep $parts $specula_read
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
######188######

specula_read=false
do_specula=false
len=0
prob_access=t
locals="1 2 3"
if [ $skip_len == 0 ] || [ $skipped == 1 ]
then
    sudo ./script/configBeforeRestart.sh $t $do_specula $len $rep $parts $specula_read 
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
######194#######

#sudo ./masterScripts/initMachnines.sh 1 benchmark_precise_fast_repl
#Test number of involved DCs

#Test number of involved DCs
######200######

### Check max throughput
