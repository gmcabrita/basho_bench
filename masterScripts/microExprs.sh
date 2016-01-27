#!/bin/bash

## Just to test.. 
#./script/runSpeculaBench.sh 4 70 20 true true 4 specula_tests
seq="1 2"
threads="16 8 4"
workloads="1 2 3"
length="8 4 2"
repl_degree="3 2 1"
start_ind=1
skip_len=139

./script/restartAndConnect.sh
for i in $seq
do
    for t in $threads
    do
	for wl in $workloads
	do
	    if [ $wl == 1 ]; then MN=13  SN=1 CN=1 
	    elif [ $wl == 2 ]; then  MN=5 SN=5 CN=5
	    elif [ $wl == 3 ]; then MN=2 SN=11 CN=2
	fi
	for wh in $warehouse
	do
	    for rep in $repl_degree
	    do
		start_ind=$((start_ind+1))
		for len in $length
		do
		    if [ $start_ind -gt $skip_len ]; then
    	    		./script/runSpeculaBench.sh $t $MN $SN $CN $MR $SR $CR true true $len random $rep specula_tests 
		    else
			echo "Skipped..."$start_ind
		    fi
	    	start_ind=$((start_ind+1))
	    	done
	    done
	done
	done
    done
done

./script/restartAndConnect.sh
for i in $seq
do
    for t in $threads
    do
	for wl in $workloads
	do
	    if [ $wl == 1 ]; then MN=13  SN=1 CN=1 
	    elif [ $wl == 2 ]; then  MN=5 SN=5 CN=5
	    elif [ $wl == 3 ]; then MN=2 SN=11 CN=2
	fi
	    for rep in $repl_degree
	    do
		start_ind=$((start_ind+1))
		if [ $start_ind -gt $skip_len ]; then
    	    	./script/runSpeculaBench.sh $t $MN $SN $CN $MR $SR $CR false false 0 random $rep specula_tests 
		else
		    echo "Skipped..."$start_ind
		fi
	    done
	done
    done
done
exit
