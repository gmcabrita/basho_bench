#!/bin/bash

## Just to test.. 
#./script/runSpeculaBench.sh 4 70 20 true true 4 specula_tests
seq="1"
threads="8"
length="1 2 4 8"
rep=2
parts=3
start_ind=1
skip_len=0
#100
skipped=1
inited=0
AM=80
AS=20

#sudo ./masterScripts/initMachnines.sh 1 benchmark_precise_fast_repl
for t in $threads
do
    for len in $length
    do
        if [ $skipped -eq 1 ] 
        then
	       sudo ./script/configBeforeRestart.sh $t true true $len $rep $parts specula
	       sudo ./script/restartAndConnect.sh
	       sleep 20
        fi

        if [ $skipped -eq 1 ]
        then
            sudo ./script/preciseTime.sh
        fi

        for i in $seq
        do
            if [ $start_ind -gt $skip_len ]; 
            then

                if [ $skipped -eq 0 ]
                then
                    skipped=1
                    echo "Configuring and starting nodes.."
                    sudo ./script/configBeforeRestart.sh $t true true $len $rep $parts specula
                    sudo ./script/restartAndConnect.sh
                    sleep 20
                fi

                ./script/runRubisBench.sh $t $AM $AS true true $len specula_tests $start_ind
                skipped=1
            else
                echo "Skipped..."$start_ind
            fi
            start_ind=$((start_ind+1))
        done
    done
done

#./script/runSpeculaBench.sh $t $AM $AS true true 1 specula_tests 4 0 0 $rep $start_ind
#start_ind=$((start_ind+1))
#./script/runSpeculaBench.sh $t $AM $AS true true 1 specula_tests 8 0 0 $rep $start_ind
#start_ind=$((start_ind+1))


#sudo ./masterScripts/initMachnines.sh 1 benchmark_no_specula
#sudo ./masterScripts/initMachnines.sh 1 benchmark_no_specula
#sudo ./script/configBeforeRestart.sh 32 false false 0 $rep $parts nospecula 
#sudo ./script/restartAndConnect.sh

threads="32 60"
for t in $threads
do  
        #sudo ./script/configBeforeRestart.sh $t false false 0 $rep $parts nospecula 
        #sudo ./script/restartAndConnect.sh
        #sleep 20 
        for i in $seq
        do
            if [ $start_ind -gt $skip_len ]; then
                sudo ./script/preciseTime.sh
                ./script/runRubisBench.sh $t $AM $AS false false 0 specula_tests $start_ind
            else
                echo "Skipped..."$start_ind
            fi  
            start_ind=$((start_ind+1))
        done
done

#./script/runSpeculaBench.sh $t $AM $AS false false 0 specula_tests 4 0 0 $rep $start_ind
#start_ind=$((start_ind+1))
#./script/runSpeculaBench.sh $t $AM $AS false false 0 specula_tests 8 0 0 $rep $start_ind
#start_ind=$((start_ind+1))
