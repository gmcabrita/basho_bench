#!/bin/bash

## Just to test.. 
#./script/runSpeculaBench.sh 4 70 20 true true 4 specula_tests
AM=80
AS=20
seq="1 2"
threads="2 4 8"
workloads="1 2 3 4"
length="1 2 4 8"
start_ind=1
skip_len=0
    	    #./script/runSpeculaBench.sh 8 $AM $AS false false 0 specula_tests 2 45 45 
    	    #./script/runSpeculaBench.sh 8 $AM $AS false false 0 specula_tests 2 45 45 
    	    #./script/runSpeculaBench.sh 16 $AM $AS false false 0 specula_tests 2 45 45 
    	    #./script/runSpeculaBench.sh 16 $AM $AS false false 0 specula_tests 2 45 45 
    	    #./script/runSpeculaBench.sh 8 $AM $AS false false 0 specula_tests 4 45 45 
    	    #./script/runSpeculaBench.sh 8 $AM $AS false false 0 specula_tests 4 45 45 
    	    #./script/runSpeculaBench.sh 16 $AM $AS false false 0 specula_tests 4 45 45 
    	    #./script/runSpeculaBench.sh 16 $AM $AS false false 0 specula_tests 4 45 45 

	#	exit
    	    ./script/runSpeculaBench.sh 30 $AM $AS false false 0 specula_tests 2 45 45 
    	    ./script/runSpeculaBench.sh 30 $AM $AS false false 0 specula_tests 2 45 45 
    	    ./script/runSpeculaBench.sh 16 $AM $AS false false 0 specula_tests 2 45 45 
    	    ./script/runSpeculaBench.sh 16 $AM $AS false false 0 specula_tests 2 45 45 
    	    ./script/runSpeculaBench.sh 30 $AM $AS false false 0 specula_tests 4 45 45 
    	    ./script/runSpeculaBench.sh 30 $AM $AS false false 0 specula_tests 4 45 45 
    	    ./script/runSpeculaBench.sh 16 $AM $AS false false 0 specula_tests 4 45 45 
    	    ./script/runSpeculaBench.sh 16 $AM $AS false false 0 specula_tests 4 45 45 
    	    ./script/runSpeculaBench.sh 30 $AM $AS false false 0 specula_tests 6 45 45 
    	    ./script/runSpeculaBench.sh 30 $AM $AS false false 0 specula_tests 6 45 45 
    	    ./script/runSpeculaBench.sh 16 $AM $AS false false 0 specula_tests 6 45 45 
    	    ./script/runSpeculaBench.sh 16 $AM $AS false false 0 specula_tests 6 45 45 

    	    ./script/runSpeculaBench.sh 16 $AM $AS true true 4 specula_tests 2 45 45 
    	    ./script/runSpeculaBench.sh 16 $AM $AS true true 4 specula_tests 2 45 45 
    	    ./script/runSpeculaBench.sh 16 $AM $AS true true 2 specula_tests 2 45 45 
    	    ./script/runSpeculaBench.sh 16 $AM $AS true true 2 specula_tests 2 45 45 
    	    ./script/runSpeculaBench.sh 16 $AM $AS true true 4 specula_tests 4 45 45 
    	    ./script/runSpeculaBench.sh 16 $AM $AS true true 4 specula_tests 4 45 45 
    	    ./script/runSpeculaBench.sh 16 $AM $AS true true 2 specula_tests 6 45 45 
    	    ./script/runSpeculaBench.sh 16 $AM $AS true true 2 specula_tests 6 45 45 
    	    ./script/runSpeculaBench.sh 16 $AM $AS true true 4 specula_tests 6 45 45 
    	    ./script/runSpeculaBench.sh 16 $AM $AS true true 4 specula_tests 6 45 45 
    	    ./script/runSpeculaBench.sh 16 $AM $AS true true 2 specula_tests 6 45 45 
    	    ./script/runSpeculaBench.sh 16 $AM $AS true true 2 specula_tests 6 45 45 
    	    ./script/runSpeculaBench.sh 16 $AM $AS true true 8 specula_tests 6 45 45 
    	    ./script/runSpeculaBench.sh 16 $AM $AS true true 8 specula_tests 6 45 45 
    	    ./script/runSpeculaBench.sh 16 $AM $AS true true 8 specula_tests 4 45 45 
    	    ./script/runSpeculaBench.sh 16 $AM $AS true true 8 specula_tests 4 45 45 
    	    ./script/runSpeculaBench.sh 16 $AM $AS true true 8 specula_tests 2 45 45 
    	    ./script/runSpeculaBench.sh 16 $AM $AS true true 8 specula_tests 2 45 45 
		exit
    	    ./script/runSpeculaBench.sh 8 $AM $AS true true 4 specula_tests 2 45 45 
    	    ./script/runSpeculaBench.sh 8 $AM $AS true true 4 specula_tests 4 45 45 
    	    ./script/runSpeculaBench.sh 8 $AM $AS true true 4 specula_tests 4 45 45 
    	    ./script/runSpeculaBench.sh 8 $AM $AS true true 8 specula_tests 2 45 45 
    	    ./script/runSpeculaBench.sh 8 $AM $AS true true 8 specula_tests 2 45 45 
    	    ./script/runSpeculaBench.sh 8 $AM $AS true true 8 specula_tests 4 45 45 
    	    ./script/runSpeculaBench.sh 8 $AM $AS true true 8 specula_tests 4 45 45 
    	    ./script/runSpeculaBench.sh 12 $AM $AS true true 2 specula_tests 2 45 45 
    	    ./script/runSpeculaBench.sh 12 $AM $AS true true 2 specula_tests 2 45 45 
    	    ./script/runSpeculaBench.sh 12 $AM $AS true true 2 specula_tests 4 45 45 
    	    ./script/runSpeculaBench.sh 12 $AM $AS true true 2 specula_tests 4 45 45 
    	    ./script/runSpeculaBench.sh 12 $AM $AS true true 4 specula_tests 2 45 45 
    	    ./script/runSpeculaBench.sh 12 $AM $AS true true 4 specula_tests 2 45 45 
    	    ./script/runSpeculaBench.sh 12 $AM $AS true true 4 specula_tests 4 45 45 
    	    ./script/runSpeculaBench.sh 12 $AM $AS true true 4 specula_tests 4 45 45 
		exit
    	    ./script/runSpeculaBench.sh 12 $AM $AS true true 4 specula_tests 2 45 45 
    	    ./script/runSpeculaBench.sh 12 $AM $AS true true 4 specula_tests 4 45 45 
    	    ./script/runSpeculaBench.sh 12 $AM $AS true true 4 specula_tests 2 45 45 
    	    ./script/runSpeculaBench.sh 12 $AM $AS true true 4 specula_tests 4 45 45 
exit
exit
for i in $seq
do
    for t in $threads
    do
	for wl in $workloads
	do
	    if [ $wl == 1 ]; then  n=45  p=45
	    elif [ $wl == 2 ]; then  n=5 p=5
	    elif [ $wl == 3 ]; then n=10 p=0
	    elif [ $wl == 4 ]; then n=90 p=0
	fi
	if [ $start_ind -gt $skip_len ]; then
    	    ./script/runSpeculaBench.sh $t $AM $AS false false 0 specula_tests 2 $n $p 
	else
	    echo "Skipped..."$start_ind
	fi
	start_ind=$((start_ind+1))
	for len in $length
	do
	    if [ $start_ind -gt $skip_len ]; then
    	        ./script/runSpeculaBench.sh $t $AM $AS true true $len specula_tests 2 $n $p
	    else
		echo "Skipped..."$start_ind
	    fi
	    start_ind=$((start_ind+1))
	done
	done
    done
done
exit
    ./script/runSpeculaBench.sh 8 $AM $AS true true 2 specula_tests 2 
    ./script/runSpeculaBench.sh 8 $AM $AS true true 2 specula_tests 2 
	exit
    ./script/runSpeculaBench.sh 2 $AM $AS false false 0 specula_tests 1 
    ./script/runSpeculaBench.sh 2 $AM $AS false false 0 specula_tests 1 
    ./script/runSpeculaBench.sh 2 $AM $AS false false 0 specula_tests 2
    ./script/runSpeculaBench.sh 2 $AM $AS false false 0 specula_tests 2 
    #./script/runSpeculaBench.sh 2 $AM $AS true true 1 specula_tests 1 
    #./script/runSpeculaBench.sh 2 $AM $AS true true 1 specula_tests 1 
    ./script/runSpeculaBench.sh 2 $AM $AS true true 1 specula_tests 2
    ./script/runSpeculaBench.sh 2 $AM $AS true true 1 specula_tests 2 
    ./script/runSpeculaBench.sh 2 $AM $AS true true 2 specula_tests 1 
    ./script/runSpeculaBench.sh 2 $AM $AS true true 2 specula_tests 1 
    ./script/runSpeculaBench.sh 2 $AM $AS true true 2 specula_tests 2
    ./script/runSpeculaBench.sh 2 $AM $AS true true 2 specula_tests 2 
    ./script/runSpeculaBench.sh 4 $AM $AS false false 0 specula_tests 1 
    ./script/runSpeculaBench.sh 4 $AM $AS false false 0 specula_tests 1 
    ./script/runSpeculaBench.sh 4 $AM $AS false false 0 specula_tests 2
    ./script/runSpeculaBench.sh 4 $AM $AS false false 0 specula_tests 2 
    ./script/runSpeculaBench.sh 4 $AM $AS true true 1 specula_tests 1 
    ./script/runSpeculaBench.sh 4 $AM $AS true true 1 specula_tests 1 
    ./script/runSpeculaBench.sh 4 $AM $AS true true 1 specula_tests 2 
    ./script/runSpeculaBench.sh 4 $AM $AS true true 1 specula_tests 2 
    ./script/runSpeculaBench.sh 4 $AM $AS true true 2 specula_tests 1 
    ./script/runSpeculaBench.sh 4 $AM $AS true true 2 specula_tests 1 
    ./script/runSpeculaBench.sh 4 $AM $AS true true 2 specula_tests 2 
    ./script/runSpeculaBench.sh 4 $AM $AS true true 2 specula_tests 2 
    ./script/runSpeculaBench.sh 4 $AM $AS true true 4 specula_tests 1 
    ./script/runSpeculaBench.sh 4 $AM $AS true true 4 specula_tests 1 
    ./script/runSpeculaBench.sh 4 $AM $AS true true 4 specula_tests 2 
    ./script/runSpeculaBench.sh 4 $AM $AS true true 4 specula_tests 2 
    ./script/runSpeculaBench.sh 8 $AM $AS false false 0 specula_tests 1 
    ./script/runSpeculaBench.sh 8 $AM $AS false false 0 specula_tests 1 
    ./script/runSpeculaBench.sh 8 $AM $AS false false 0 specula_tests 2
    ./script/runSpeculaBench.sh 8 $AM $AS false false 0 specula_tests 2 
    ./script/runSpeculaBench.sh 8 $AM $AS true true 1 specula_tests 1 
    ./script/runSpeculaBench.sh 8 $AM $AS true true 1 specula_tests 1 
    ./script/runSpeculaBench.sh 8 $AM $AS true true 1 specula_tests 2 
    ./script/runSpeculaBench.sh 8 $AM $AS true true 1 specula_tests 2 
    ./script/runSpeculaBench.sh 8 $AM $AS true true 2 specula_tests 1 
    ./script/runSpeculaBench.sh 8 $AM $AS true true 2 specula_tests 1 
    ./script/runSpeculaBench.sh 8 $AM $AS true true 2 specula_tests 2 
    ./script/runSpeculaBench.sh 8 $AM $AS true true 2 specula_tests 2 
exit

AccessMaster="50 80 0"
for AM in $AccessMaster
do
    AS=$((100-AM))
    echo "Access master is " $AM ", Access slave is "$AS
    ./script/runSpeculaBench.sh 1 $AM $AS false false 0 specula_tests
    ./script/runSpeculaBench.sh 1 $AM $AS false false 0 specula_tests
    ./script/runSpeculaBench.sh 2 $AM $AS false false 0 specula_tests
    ./script/runSpeculaBench.sh 2 $AM $AS false false 0 specula_tests
    ./script/runSpeculaBench.sh 4 $AM $AS false false 0 specula_tests
    ./script/runSpeculaBench.sh 4 $AM $AS false false 0 specula_tests
    ./script/runSpeculaBench.sh 1 $AM $AS true false 1 specula_tests
    ./script/runSpeculaBench.sh 1 $AM $AS true false 1 specula_tests
    ./script/runSpeculaBench.sh 1 $AM $AS true false 2 specula_tests
    ./script/runSpeculaBench.sh 1 $AM $AS true false 2 specula_tests
    ./script/runSpeculaBench.sh 1 $AM $AS true false 4 specula_tests
    ./script/runSpeculaBench.sh 1 $AM $AS true false 4 specula_tests
    ./script/runSpeculaBench.sh 1 $AM $AS true false 8 specula_tests
    ./script/runSpeculaBench.sh 1 $AM $AS true false 8 specula_tests
    ./script/runSpeculaBench.sh 1 $AM $AS true true 1 specula_tests
    ./script/runSpeculaBench.sh 1 $AM $AS true true 1 specula_tests
    ./script/runSpeculaBench.sh 1 $AM $AS true true 2 specula_tests
    ./script/runSpeculaBench.sh 1 $AM $AS true true 2 specula_tests
    ./script/runSpeculaBench.sh 1 $AM $AS true true 4 specula_tests
    ./script/runSpeculaBench.sh 1 $AM $AS true true 4 specula_tests
    ./script/runSpeculaBench.sh 1 $AM $AS true true 8 specula_tests
    ./script/runSpeculaBench.sh 1 $AM $AS true true 8 specula_tests
    ./script/runSpeculaBench.sh 2 $AM $AS true true 1 specula_tests
    ./script/runSpeculaBench.sh 2 $AM $AS true true 1 specula_tests
    ./script/runSpeculaBench.sh 2 $AM $AS true true 2 specula_tests
    ./script/runSpeculaBench.sh 2 $AM $AS true true 2 specula_tests
    ./script/runSpeculaBench.sh 2 $AM $AS true true 4 specula_tests
    ./script/runSpeculaBench.sh 2 $AM $AS true true 4 specula_tests
    ./script/runSpeculaBench.sh 2 $AM $AS true true 8 specula_tests
    ./script/runSpeculaBench.sh 2 $AM $AS true true 8 specula_tests
done 
exit
#./script/runSpeculaBench.sh 1 100 0 false false 0 specula_tests
#./script/runSpeculaBench.sh 1 100 0 false false 0 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 0 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 0 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 0 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 0 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 0 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 0 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 1 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 1 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 1 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 1 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 1 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 1 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 2 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 2 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 2 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 2 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 2 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 2 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 4 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 4 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 4 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 4 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 4 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 4 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 8 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 8 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 8 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 8 specula_tests

exit

./script/runSpeculaBench.sh 1 0 100 false false 0 specula_tests
./script/runSpeculaBench.sh 2 0 100 false false 0 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 5 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 5 specula_tests
#./script/runSpeculaBench.sh 2 100 0 true false 5 specula_tests
#./script/runSpeculaBench.sh 2 100 0 true true 5 specula_tests
./script/runSpeculaBench.sh 1 0 100 true false 5 specula_tests
./script/runSpeculaBench.sh 1 0 100 true false 5 specula_tests
./script/runSpeculaBench.sh 1 0 100 true true 5 specula_tests
./script/runSpeculaBench.sh 1 0 100 true true 5 specula_tests
./script/runSpeculaBench.sh 2 0 100 true false 5 specula_tests
./script/runSpeculaBench.sh 2 0 100 true false 5 specula_tests
./script/runSpeculaBench.sh 2 0 100 true true 5 specula_tests
./script/runSpeculaBench.sh 2 0 100 true true 5 specula_tests
exit

# Only update replica 
./script/runSpeculaBench.sh 1 0 100 true 0 specula_tests
./script/runSpeculaBench.sh 1 0 100 true 0 specula_tests
./script/runSpeculaBench.sh 1 0 100 true 0 specula_tests
./script/runSpeculaBench.sh 1 0 100 true 1 specula_tests
./script/runSpeculaBench.sh 1 0 100 true 1 specula_tests
./script/runSpeculaBench.sh 1 0 100 true 1 specula_tests
./script/runSpeculaBench.sh 1 0 100 true 2 specula_tests
./script/runSpeculaBench.sh 1 0 100 true 2 specula_tests
./script/runSpeculaBench.sh 1 0 100 true 2 specula_tests
./script/runSpeculaBench.sh 1 0 100 true 4 specula_tests
./script/runSpeculaBench.sh 1 0 100 true 4 specula_tests
./script/runSpeculaBench.sh 1 0 100 true 4 specula_tests
./script/runSpeculaBench.sh 1 0 100 false 1 specula_tests
./script/runSpeculaBench.sh 1 0 100 false 1 specula_tests
./script/runSpeculaBench.sh 1 0 100 false 1 specula_tests

#Only update other 
#./script/runSpeculaBench.sh 1 0 0 true 1 specula_tests
#./script/runSpeculaBench.sh 1 0 0 true 2 specula_tests
#./script/runSpeculaBench.sh 1 0 0 true 4 specula_tests
#./script/runSpeculaBench.sh 1 0 0 true 1 specula_tests
#./script/runSpeculaBench.sh 1 0 0 true 2 specula_tests
#./script/runSpeculaBench.sh 1 0 0 true 4 specula_tests
#./script/runSpeculaBench.sh 1 0 0 false 1 specula_tests

#Low locality 
#./script/runSpeculaBench.sh 1 0 20 true 1 specula_tests
#./script/runSpeculaBench.sh 1 0 20 true 2 specula_tests
#./script/runSpeculaBench.sh 1 0 20 true 4 specula_tests
#./script/runSpeculaBench.sh 1 0 20 true 1 specula_tests
#./script/runSpeculaBench.sh 1 0 20 true 2 specula_tests
#./script/runSpeculaBench.sh 1 0 20 true 4 specula_tests
#./script/runSpeculaBench.sh 1 0 20 false 1 specula_tests

# High locality 
#./script/runSpeculaBench.sh 1 80 4 true 0 specula_tests
#./script/runSpeculaBench.sh 1 80 4 true 1 specula_tests
#./script/runSpeculaBench.sh 1 80 4 true 2 specula_tests
#./script/runSpeculaBench.sh 1 80 4 true 4 specula_tests
#./script/runSpeculaBench.sh 1 80 4 false 1 specula_tests

#./script/runSpeculaBench.sh 4 80 4 true 1 specula_tests
#./script/runSpeculaBench.sh 4 80 4 true 2 specula_tests
exit
