#!/bin/bash
function runTpccNTimes {
    for i in $seq
    do
        if [ $start_ind -gt $skip_len ]; then
        ./script/runSpeculaBench.sh $t $AM $AS $do_specula $think_time $len specula_tests $wh $n $p $rep $start_ind
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
        ./script/runRubisBench.sh $t $AM $AC $do_specula $think_time $len specula_tests $start_ind
        #echo $t $MN $SN $CN $MR $SR $CR $do_specula $len random $rep $comp specula_tests $start_ind
        skipped=1
        else
        echo "Skipped..."$start_ind
        fi
        start_ind=$((start_ind+1))
    done
}

## Just to test.. 
seq="1"
threads="64 128 256 384 512"
workloads="1 2 3 4"
length="8"
warehouse="2"

think_times="tpcc"

#rep=8
#parts=28
#rep=5
#parts=20
rep=2
parts=4

start_ind=1
skip_len=0
skipped=1
AM=80
AS=0

specula_read=true
do_specula=true

sudo ./masterScripts/initMachnines.sh 1 benchmark_precise_fast_repl
sudo ./script/parallel_command.sh "cd antidote && sudo make rel"

rm -rf ./config
echo micro cdf true >> config
echo micro duration 180 >> config
echo ant cdf true >> ./config
sudo ./script/copy_to_all.sh ./config ./basho_bench/
sudo ./script/parallel_command.sh "cd basho_bench && sudo ./script/config_by_file.sh"

sudo ./script/configBeforeRestart.sh 8 $do_specula 8 $rep $parts $specula_read
sudo ./script/restartAndConnect.sh

for t in $threads
do
    for think_time in $think_times
    do
        for len in $length
        do
            if [ $skipped -eq 1 ] 
            then
	        sudo ./script/configBeforeRestart.sh $t $do_specula $len $rep $parts $specula_read
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
                    think_time="tpcc"
                    runTpccNTimes
	            done
	        done
            think_time="rubis"
            runRubis
        done
    done
done

specula_read=false
do_specula=false
len=0
sudo ./masterScripts/initMachnines.sh 1 benchmark_no_specula
sudo ./script/parallel_command.sh "cd antidote && sudo make rel"

rm -rf ./config
echo micro cdf true >> config
echo micro duration 180 >> config
echo ant cdf true >> ./config
sudo ./script/copy_to_all.sh ./config ./basho_bench/
sudo ./script/parallel_command.sh "cd basho_bench && sudo ./script/config_by_file.sh"

sudo ./script/configBeforeRestart.sh 64 $do_specula 0 $rep $parts $specula_read 
sudo ./script/restartAndConnect.sh


for t in $threads
do  
    for think_time in $think_times
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
                think_time="tpcc"
                runTpccNTimes 
            done
        done
        think_time="rubis"
        runRubis
    done
done
