#!/bin/bash

function runNTimes {
    for i in $seq
    do
        if [ $start_ind -gt $skip_len ]; then
        ./script/runMicroBench.sh $t $MN $SN $CN $MR $SR $CR $do_specula $len random $rep $comp specula_tests $start_ind
        #echo $t $MN $SN $CN $MR $SR $CR $do_specula $len random $rep $comp specula_tests $start_ind
        skipped=1
        else
        echo "Skipped..."$start_index
        fi
        start_ind=$((start_ind+1))
    done
} 

if [ $1 == true ]
then
    do_specula=true
    fast_reply=true
    length="8 4 2 1"
else
    do_specula=false
    fast_reply=false
    length="0"
fi
## Just to test.. 
seq="1 2"
t="8"
#workloads="1 2 3 5"
contentions="1 2 3"
localities="1 2 3"
local_comp="100 500 1000"
replications="4 5 6 7 8"
start_ind=1
skipped=0
skip_len=$2
parts=54
BIG=1000000
MID=100000
SML=20000

rep=3
for len in $length
do
    if [ $skip_len == 0 ] || [ $skipped == 1 ]
    then
    sudo ./script/configBeforeRestart.sh $t $do_specula $fast_reply $len $rep $parts
    sudo ./script/restartAndConnect.sh
    sleep 25
    fi
    for cont in $contentions
    do
        if [ $cont == 1 ]; then MR=$BIG SR=$BIG CR=$BIG
        elif [ $cont == 2 ]; then MR=$MID SR=$MID CR=$MID
        elif [ $cont == 3 ]; then  MR=$SML SR=$SML CR=$SML
        fi
        MN=12    SN=0    CN=3
	    for comp in $local_comp
	    do
            if [ $skip_len == 0 ] || [ $skipped == 1 ]
            then
	        sudo ./script/preciseTime.sh
            fi
            runNTimes
        done
	        
        comp=0
	    for locality in $localities
        do
		    if [ $locality == 1 ]; then MN=12 SN=0 CN=3
            elif [ $locality == 2 ]; then MN=8 SN=0 CN=7 
            elif [ $locality == 3 ]; then MN=3 SN=0 CN=12 
            fi
            if [ $skip_len == 0 ] || [ $skipped == 1 ]
            then
            sudo ./script/preciseTime.sh
            fi
            runNTimes
        done
    done
done


comp=0
MN=3   SN=0  CN=12
for len in $length
do
    for rep in $replications
    do
        if [ $skip_len == 0 ] || [ $skipped == 1 ]
        then
        sudo ./script/configBeforeRestart.sh $t $do_specula $fast_reply $len $rep $parts 
        sudo ./script/restartAndConnect.sh
        sleep 25
        fi
        for cont in $contentions
        do
            if [ $cont == 1 ]; then MR=$BIG SR=$BIG CR=$BIG
            elif [ $cont == 2 ]; then MR=$MID SR=$MID CR=$MID
            elif [ $cont == 3 ]; then  MR=$SML SR=$SML CR=$SML
            fi
            if [ $skip_len == 0 ] || [ $skipped == 1 ]
            then
            sudo ./script/preciseTime.sh
            fi
            runNTimes
        done
    done
done


echo "Finish speculation"
