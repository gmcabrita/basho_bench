#!/bin/bash

## Just to test.. 
#./script/runSpeculaBench.sh 4 70 20 true true 4 specula_tests
seq="1"
threads="16 8 4"
workloads="1 2 3"
length="8 4 2"
repl_degree="3 2 1"
start_ind=1
skip_len=0

#./script/restartAndConnect.sh
for len in $length
do
sudo ./script/restartAndConnect.sh
    for t in $threads
    do
	for wl in $workloads
	do
	    if [ $wl == 1 ]; then MN=8  SN=1 CN=1  MR=100000 SR=100000 CR=100000 
	    elif [ $wl == 2 ]; then MN=8 SN=1 CN=1  MR=10000 SR=100000 CR=100000 
	    elif [ $wl == 3 ]; then MN=2 SN=6 CN=2 MR=100000 SR=100000 CR=100000
	    elif [ $wl == 4 ]; then MN=2 SN=6 CN=2 MR=100000 SR=10000 CR=100000
	    fi
	    for rep in $repl_degree
	    do
		    for i in $seq
		    do
			    if [ $start_ind -gt $skip_len ]; then
				./script/runMicroBench.sh $t $MN $SN $CN $MR $SR $CR true $len random $rep specula_tests false
			    else
				echo "Skipped..."$start_ind
			    fi
		    done
	    	start_ind=$((start_ind+1))
	    done
	done
     done
done


#echo "Trying to run non-specula!"
#./script/restartAndConnect.sh
for i in $seq
do
    for t in $threads
    do
	for wl in $workloads
	do
	    if [ $wl == 1 ]; then MN=8  SN=1 CN=1  MR=100000 SR=100000 CR=100000
            elif [ $wl == 2 ]; then MN=8 SN=1 CN=1  MR=10000 SR=100000 CR=100000  
            elif [ $wl == 3 ]; then MN=2 SN=6 CN=2 MR=100000 SR=100000 CR=100000
            elif [ $wl == 4 ]; then MN=2 SN=6 CN=2 MR=100000 SR=10000 CR=100000
            fi
	    for rep in $repl_degree
	    do
		start_ind=$((start_ind+1))
		if [ $start_ind -gt $skip_len ]; then
    	    	./script/runMicroBench.sh $t $MN $SN $CN $MR $SR $CR false 0 random $rep specula_tests false 
		else
		    echo "Skipped..."$start_ind
		fi
	    done
	done
    done
done
exit
