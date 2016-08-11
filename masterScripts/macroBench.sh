#!/bin/bash
function runTpccNTimes {
    for i in $seq
    do
        if [ $start_ind -gt $skip_len ]; then
	sudo ./script/preciseTime.sh
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
	sudo ./script/preciseTime.sh
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
warehouse="5"

#rep=5
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

tpcc_length="0"
rubis_length="0"
warehouse="5"

specula_read=false
do_specula=false
len=0
sudo ./masterScripts/initMachnines.sh 1 benchmark_no_specula_nodict_optsup
sudo ./script/parallel_command.sh "cd antidote && sudo make rel"

rm -rf ./config
echo tpcc cdf true >> config
echo tpcc duration 120 >> config
echo rubis cdf true >> config
echo rubis duration 120 >> config
echo ant cdf true >> ./config
sudo ./script/copy_to_all.sh ./config ./basho_bench/
sudo ./script/parallel_command.sh "cd basho_bench && sudo ./script/config_by_file.sh"

sudo ./script/configBeforeRestart.sh 4000 $do_specula 0 $rep $parts $specula_read 
sudo ./script/restartAndConnect.sh true

rubis_threads="4000 5000"
seq="1"
for t in $rubis_threads
do  
        think_time="rubis"
        runRubis
done

############# Specula here

specula_read=true
do_specula=true

tpcc_length="0"
rubis_length="0"

sudo ./masterScripts/initMachnines.sh 1 benchmark_precise_nodict_optsup
sudo ./script/parallel_command.sh "cd antidote && sudo make rel"

rm -rf ./config
echo tpcc cdf true >> config
echo tpcc duration 120 >> config
echo rubis cdf true >> config
echo rubis duration 120 >> config
echo ant cdf true >> ./config
sudo ./script/copy_to_all.sh ./config ./basho_bench/
sudo ./script/parallel_command.sh "cd basho_bench && sudo ./script/config_by_file.sh"

sudo ./script/configBeforeRestart.sh 5000 $do_specula 0 $rep $parts $specula_read
sudo ./script/restartAndConnect.sh true

len=0

rubis_threads="4000 5000"
seq="1"
for t in $rubis_threads
do  
        think_time="rubis"
        runRubis
done


tpcc_threads="1200"
#tpcc_threads="800 1000 200 400"
workloads="1"

seq="1"
for t in $tpcc_threads
do
        for len in $tpcc_length
        do
	  	for wl in $workloads
	        do
	            if [ $wl == 1 ]; then  n=45  p=43
	            elif [ $wl == 2 ]; then  n=5 p=83
	            elif [ $wl == 3 ]; then n=5 p=43
	            fi
	            for wh in $warehouse
	            do
                    think_time="tpcc"
                    runTpccNTimes
	            done
	        done
        done
done
