#!/bin/bash

## Just to test.. 
#./script/runSpeculaBench.sh 4 70 20 true true 4 specula_tests
seq="1"
threads="8"
workloads="5 6"
#workloads="5"
length="1 2 4 8"
warehouse="4 8"
rep=2
parts=3
start_ind=1
skip_len=12
skipped=0
AM=80
AS=0

#th=8
#len=1
#wh=4
#sudo ./script/configBeforeRestart.sh $th true true $len $rep $parts
#sudo ./script/restartAndConnect.sh
#./script/runSpeculaBench.sh $th $AM $AS true true $len specula_tests $wh 0 100 $rep 0
#exit

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
	    elif [ $wl == 6 ]; then n=100 p=0
	    elif [ $wl == 7 ]; then n=0 p=0
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

./script/runSpeculaBench.sh $t $AM $AS true true 1 specula_tests 4 0 0 $rep $start_ind
start_ind=$((start_ind+1))
./script/runSpeculaBench.sh $t $AM $AS true true 1 specula_tests 8 0 0 $rep $start_ind
start_ind=$((start_ind+1))

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
	        elif [ $wl == 5 ]; then n=0 p=100
	        elif [ $wl == 6 ]; then n=100 p=0
	        elif [ $wl == 7 ]; then n=0 p=0
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
