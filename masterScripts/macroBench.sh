#!/bin/bash
function runTpccNTimes {
    for i in $seq
    do
        if [ $start_ind -gt $skip_len ]; then
        ./script/runSpeculaBench.sh $t $AM $AS $do_specula $fast_reply $len specula_tests $wh $n $p $rep $start_ind
        skipped=1
        else
        echo "Skipped..."$start_ind
        fi
        start_ind=$((start_ind+1))
     done
}

function runRubis {
    for i in $seq
    do
        if [ $start_ind -gt $skip_len ]; then
        AC=$((100-AM-AS))
        ./script/runRubisBench.sh $t $AM $AC $do_specula $specula_read $len specula_tests $start_ind
        #echo $t $MN $SN $CN $MR $SR $CR $do_specula $len random $rep $comp specula_tests $start_ind
        skipped=1
        else
        echo "Skipped..."$start_ind
        fi
        start_ind=$((start_ind+1))
    done
}

## Just to test.. 
seq="1 2"
threads="8"
workloads="1 2 3 4"
length="8"
warehouse="2"

#rep=8
#parts=28
#rep=5
#parts=20
rep=1
parts=12

start_ind=1
skip_len=0
skipped=0
inited=0
AM=80
AS=0

specula_read=specula
do_specula=true
fast_reply=true

t=8
len=8
#sudo ./masterScripts/initMachnines.sh 1 benchmark_precise_fast_repl
#sudo ./script/parallel_command.sh "cd antidote && sudo make rel"
sudo ./script/configBeforeRestart.sh $t $do_specula $fast_reply 8 $rep $parts $specula_read
sudo ./script/restartAndConnect.sh

for t in $threads
do
    for len in $length
    do
    if [ $skipped -eq 1 ] 
    then
	sudo ./script/configBeforeRestart.sh $t $do_specula $fast_reply $len $rep $parts $specula_read
    fi
	for wl in $workloads
	do
	    if [ $wl == 1 ]; then  n=9  p=1
	    elif [ $wl == 2 ]; then  n=1 p=9
	    elif [ $wl == 3 ]; then n=80 p=10
	    elif [ $wl == 4 ]; then n=10 p=80
	    fi
	    for wh in $warehouse
	    do
            if [ $skipped -eq 1 ]
            then
		        sudo ./script/preciseTime.sh
            fi
            runTpccNTimes
	    done
	done
    done
done
runRubis

sudo ./masterScripts/initMachnines.sh 1 benchmark_no_specula
specula_read=nospecula
do_specula=false
fast_reply=false
len=8
sudo ./script/parallel_command.sh "cd antidote && sudo make rel"
sudo ./script/configBeforeRestart.sh 8 $do_specula $fast_reply 0 $rep $parts $specula_read 
sudo ./script/restartAndConnect.sh
for t in $threads
do  
        for wl in $workloads
        do
	    if [ $wl == 1 ]; then  n=9  p=1
            elif [ $wl == 2 ]; then  n=1 p=9
            elif [ $wl == 3 ]; then n=80 p=10
            elif [ $wl == 4 ]; then n=10 p=80
            fi
            for wh in $warehouse
            do
                runTpccNTimes 
            done
        done
done
runRubis
