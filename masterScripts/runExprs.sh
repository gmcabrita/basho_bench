#!/bin/bash

## Just to test.. 
#./script/runSpeculaBench.sh 4 70 20 true true 4 specula_tests
seq="1"
threads="8"
workloads="1 2 3 4"
#workloads="5"
length=""
warehouse="2 4"
rep=2
parts=3
start_ind=1
skip_len=0
skipped=1
AM=80
AS=0

for t in $threads
do
    for len in $length
    do
    if [ $skipped -eq 1 ] 
    then
	sudo ./script/configBeforeRestart.sh $t true true $len $rep $parts
	sudo ./script/restartAndConnect.sh
	sleep 20
    fi
	for wl in $workloads
	do
	    if [ $wl == 1 ]; then  n=9  p=1
	    elif [ $wl == 2 ]; then  n=1 p=9
	    elif [ $wl == 3 ]; then n=80 p=10
	    elif [ $wl == 4 ]; then n=10 p=80
	    elif [ $wl == 5 ]; then n=0 p=100
	    fi
	    for wh in $warehouse
	    do
		sudo ./script/preciseTime.sh
	        for i in $seq
	        do
		    if [ $start_ind -gt $skip_len ]; then
			./script/runSpeculaBench.sh $t $AM $AS true true $len specula_tests $wh $n $p $rep $start_ind
            skipped=1
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
        sleep 20 
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
                        ./script/runSpeculaBench.sh $t $AM $AS false false 0 specula_tests $wh $n $p $rep $start_ind
                    else
                        echo "Skipped..."$start_ind
                    fi  
                    start_ind=$((start_ind+1))
                done
            done
        done
done
