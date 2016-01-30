#!/bin/bash

## Just to test.. 
seq="1"
threads="16 8"
workloads="1 2 3 5"
local_comp="0 10 100"
length="8 2"
rep="3"
start_ind=1
skip_len=0
skip_mode=0
parts=36
BIG=1000000
MID=50000
SML=10000
#./script/restartAndConnect.sh
for len in $length
do
    for t in $threads
    do
	sudo ./script/configBeforeRestart.sh $t true true $len $rep $parts
        sudo ./script/restartAndConnect.sh
        sleep 10
        for wl in $workloads
        do
            if [ $wl == 1 ]; then MN=10  SN=3 CN=2  MR=BIG SR=BIG CR=BIG
            elif [ $wl == 2 ]; then MN=5 SN=5 CN=5  MR=BIG SR=SML CR=SML
            elif [ $wl == 3 ]; then MN=5 SN=0 CN=10  MR=BIG SR=SML CR=MID
            elif [ $wl == 4 ]; then MN=10 SN=3 CN=2  MR=MID SR=MID CR=MID
            elif [ $wl == 5 ]; then MN=10 SN=3 CN=2  MR=SML SR=SML CR=SML
            fi
	    for comp in $local_comp
	    do
	    sudo ./script/preciseTime.sh
	    for i in $seq
	    do
		if [ $start_ind -gt $skip_len ]; then
		    ./script/runMicroBench.sh $t $MN $SN $CN $MR $SR $CR true $len random $rep $comp specula_tests $start_ind
		else
		    echo "Skipped..."$start_ind
		fi
		start_ind=$((start_ind+1))
	    done
	    done
     done
     done
done

for t in $threads
do
    sudo ./script/configBeforeRestart.sh $t false false 0 $rep $parts
    sudo ./script/restartAndConnect.sh
    sleep 10
    for wl in $workloads
    do
	if [ $wl == 1 ]; then MN=10  SN=3 CN=2  MR=BIG SR=BIG CR=BIG
        elif [ $wl == 2 ]; then MN=5 SN=5 CN=5  MR=BIG SR=SML CR=SML
        elif [ $wl == 3 ]; then MN=5 SN=0 CN=10  MR=BIG SR=SML CR=MID
        elif [ $wl == 4 ]; then MN=10 SN=3 CN=2  MR=MID SR=MID CR=MID
        elif [ $wl == 5 ]; then MN=10 SN=3 CN=2  MR=SML SR=SML CR=SML
        fi
	for comp iin $local_comp
	do
	sudo ./script/preciseTime.sh
        for i in $seq 
        do      
            if [ $start_ind -gt $skip_len ]; then
               ./script/runMicroBench.sh $t $MN $SN $CN $MR $SR $CR false 0 random $rep $comp specula_tests $start_ind
            else
               echo "Skipped..."$start_ind
            fi
            start_ind=$((start_ind+1))
        done    
	done
     done    
done
