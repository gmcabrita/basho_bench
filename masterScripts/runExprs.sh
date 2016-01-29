#!/bin/bash

## Just to test.. 
#./script/runSpeculaBench.sh 4 70 20 true true 4 specula_tests
seq="1 2 3"
threads="16 8"
workloads="1 2 3 4"
length="8 4 2 1"
warehouse="4 2"
rep=3
parts=36
start_ind=1
skip_len=33
AM=80
AS=10

for t in $threads
do
    for len in $length
    do
	sudo ./script/configBeforeRestart.sh $t true true $len $rep $parts
	sudo ./script/restartAndConnect.sh
	sleep 10
	for wl in $workloads
	do
	    if [ $wl == 1 ]; then  n=9  p=1
	    elif [ $wl == 2 ]; then  n=1 p=9
	    elif [ $wl == 3 ]; then n=80 p=10
	    elif [ $wl == 4 ]; then n=10 p=80
	    fi
	    for wh in $warehouse
	    do
		sudo ./script/preciseTime.sh
	        for i in $seq
	        do
		    if [ $start_ind -gt $skip_len ]; then
			./script/runSpeculaBench.sh $t $AM $AS true true $len specula_tests $wh $n $p $rep $start_ind
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
	    if [ $wl == 1 ]; then  n=9  p=1
            elif [ $wl == 2 ]; then  n=1 p=9
            elif [ $wl == 3 ]; then n=80 p=10
            elif [ $wl == 4 ]; then n=10 p=80
            fi
            for wh in $warehouse
            do
		sudo ./script/preciseTime.sh
                for i in $seq
                do
                    if [ $start_ind -gt $skip_len ]; then
                        ./script/runSpeculaBench.sh $t $AM $AS true true $len specula_tests $wh $n $p $rep $start_ind
                    else
                        echo "Skipped..."$start_ind
                    fi  
                    start_ind=$((start_ind+1))
                done
            done
        done
done
