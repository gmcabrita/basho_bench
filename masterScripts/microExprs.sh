#!/bin/bash

## Just to test.. 
#./script/runSpeculaBench.sh 4 70 20 true true 4 specula_tests
seq="1"
threads="16 8"
workloads="1 2 3"
length="8 4 2"
repl_degree="3 2 1"
start_ind=1
skip_len=66
skip_mode=1
#sudo ./script/restartAndConnect.sh
MN=8 SN=1 CN=1 MR=5000 SR=50000 CR=50000
./script/runMicroBench.sh 16 $MN $SN $CN $MR $SR $CR true 8 random 1 specula_tests false 1 
./script/runMicroBench.sh 16 $MN $SN $CN $MR $SR $CR true 8 random 2 specula_tests false 1 
./script/runMicroBench.sh 16 $MN $SN $CN $MR $SR $CR false 0 random 2 specula_tests false 1 
./script/runMicroBench.sh 16 $MN $SN $CN $MR $SR $CR false 0 random 2 specula_tests false 1 
exit

#./script/restartAndConnect.sh
for len in $length
do
if [ $skip_mode -eq 0 ]
then
sudo ./script/restartAndConnect.sh
sleep 39 
fi
    for t in $threads
    do
    	for rep in $repl_degree
    	do
	    for wl in $workloads
	    do
	    if [ $wl == 1 ]; then MN=8  SN=1 CN=1  MR=100000 SR=100000 CR=100000 
	    elif [ $wl == 2 ]; then MN=8 SN=1 CN=1  MR=10000 SR=100000 CR=100000 
	    elif [ $wl == 3 ]; then MN=2 SN=6 CN=2 MR=100000 SR=100000 CR=100000
	    elif [ $wl == 4 ]; then MN=2 SN=6 CN=2 MR=100000 SR=10000 CR=100000
	    fi
		    for i in $seq
		    do
			    if [ $start_ind -gt $skip_len ]; then
				if [ $skip_mode -eq 1 ]
				then
				    sudo ./script/restartAndConnect.sh
				    sleep 35
				    skip_mode=0
				fi
				./script/runMicroBench.sh $t $MN $SN $CN $MR $SR $CR true $len random $rep specula_tests false $start_ind
			    else
				echo "Skipped..."$start_ind
			    fi
	    		    start_ind=$((start_ind+1))
		    done
	     done
	 done
     done
done


#echo "Trying to run non-specula!"
./script/restartAndConnect.sh
sleep 35 
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
    	    	./script/runMicroBench.sh $t $MN $SN $CN $MR $SR $CR false 0 random $rep specula_tests false $start_ind 
		else
		    echo "Skipped..."$start_ind
		fi
	    done
	done
    done
done
exit
