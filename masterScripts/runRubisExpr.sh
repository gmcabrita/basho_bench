#!/bin/bash

function runRubis {
    for i in $seq
    do
        if [ $start_ind -gt $skip_len ]; then
        ./script/runRubisBench.sh $t $AM $AS $do_specula $specula_read $len specula_tests $start_ind
        #echo $t $MN $SN $CN $MR $SR $CR $do_specula $len random $rep $comp specula_tests $start_ind
        skipped=1
        else
        echo "Skipped..."$start_ind
        fi
        start_ind=$((start_ind+1))
    done
}

## Just to test.. 
#./script/runSpeculaBench.sh 4 70 20 true true 4 specula_tests
seq="1"
threads="8"
length="8"
rep=2
parts=4
start_ind=1
skip_len=0
#100
skipped=1
inited=0
AM=80
AS=20

#sudo ./masterScripts/initMachnines.sh 1 benchmark_precise_fast_repl
sudo ./masterScripts/initMachnines.sh 1 benchmark_precise_choose_specula 
#sudo ./masterScripts/initMachnines.sh 1  benchmark_no_specula
#sudo ./script/stopNodes.sh
sudo ./script/parallel_command.sh "cd antidote && sudo make rel"
do_specula=true
specula_reads="true"
fast_reply=true
sudo ./script/configBeforeRestart.sh 8 $do_specula $fast_reply 8 $rep $parts true 
sudo ./script/restartAndConnect.sh
for t in $threads
do
    for len in $length
    do
	for specula_read in $specula_reads
	do
            if [ $skipped -eq 1 ] 
            then
	       sudo ./script/configBeforeRestart.sh $t $do_specula $fast_reply $len $rep $parts $specula_read 
	       #sudo ./script/restartAndConnect.sh
               sudo ./script/preciseTime.sh
	       #sleep 20
            fi
            runRubis
	done
    done
done

exit

sudo ./masterScripts/initMachnines.sh 1 benchmark_no_specula
do_specula=false
specula_read=false
fast_reply=false
len=0
for t in $threads
do
    if [ $skipped -eq 1 ] 
    then
       sudo ./script/configBeforeRestart.sh $t $do_specula $fast_reply $len $rep $parts $specula_read
       sudo ./script/restartAndConnect.sh
       sudo ./script/preciseTime.sh
       sleep 20
    fi
    runRubis
done
