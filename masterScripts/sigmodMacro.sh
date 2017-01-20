#!/bin/bash
function runTpccNTimes {
    for i in $seq
    do
        if [ $start_ind -gt $skip_len ]; then
	sudo ./script/preciseTime.sh
	if [ $wl == 1 ]; then n=5  p=83
	elif [ $wl == 2 ]; then  n=5 p=43
	elif [ $wl == 3 ]; then n=45 p=43
	fi
        ./script/runSpeculaBench.sh $t $AM $AS $do_specula $think_time $len $Folder $wh $n $p $rep $start_ind
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
        ./script/runRubisBench.sh $t $AM $AC $do_specula $think_time $len $Folder $start_ind
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

#rep=8
#parts=28
rep=5
parts=28
##rep=3
#parts=20
#rep=1
#parts=2

start_ind=1
skip_len=0
skipped=1
AM=80
AS=0

specula_read=true
do_specula=true

tpcc_length="8"
rubis_length="8"
len=8

sudo ./masterScripts/initMachnines.sh 1 benchmark_precise_remove_stat_forward_rr 
sudo ./script/parallel_command.sh "cd antidote && sudo make rel"

rm -rf ./config
echo tpcc cdf true >> config
echo tpcc duration 120 >> config
echo rubis cdf true >> config
echo rubis duration 120 >> config
echo rubis all_update true >> config
echo ant cdf true >> ./config
sudo ./script/copy_to_all.sh ./config ./basho_bench/
sudo ./script/parallel_command.sh "cd basho_bench && sudo ./script/config_by_file.sh"

sudo ./script/configBeforeRestart.sh 4000 $do_specula $len $rep $parts $specula_read
sudo ./script/restartAndConnect.sh

if [ 1 == 2 ];
then
Folder="./specula_tests/macro/external/tpcc/"
tpcc_threads="10 100 200 400 600 800"
workloads="1"
for t in $tpcc_threads
do  
        for wl in $workloads
        do
            for wh in $warehouse
            do
                think_time="tpcc"
                runTpccNTimes 
            done
        done
done

tpcc_threads="20 200 400 600 800 1000"
workloads="2"
for t in $tpcc_threads
do  
        for wl in $workloads
        do
            for wh in $warehouse
            do
                think_time="tpcc"
                runTpccNTimes 
            done
        done
done

tpcc_threads="20 200 400 800 1200 1600"
workloads="3"
for t in $tpcc_threads
do  
        for wl in $workloads
        do
	    for len in $tpcc_length
	    do
            for wh in $warehouse
            do
		for len in $tpcc_length
		do
                think_time="tpcc"
                runTpccNTimes 
		done
            done
	    done
        done
done
fi

Folder="./specula_tests/macro/external/rubis/"
#rubis_threads="50 500 1000 2000 3000 4000"
rubis_threads="3000 4000 5000 6000"
for t in $rubis_threads
do  
        think_time="rubis"
        runRubis
done


## Internal!!!
tpcc_length="0"
rubis_length="0"
len=0
sudo ./script/configBeforeRestart.sh 4000 $do_specula $len $rep $parts $specula_read
#sudo ./script/restartAndConnect.sh

if [ 1 == 2 ];
then
Folder="./specula_tests/macro/internal/tpcc/"
tpcc_threads="10 100 200 400 600 800"
workloads="1"
for t in $tpcc_threads
do  
        for wl in $workloads
        do
            for wh in $warehouse
            do
                think_time="tpcc"
                runTpccNTimes 
            done
        done
done

tpcc_threads="20 200 400 600 800 1000"
workloads="2"
for t in $tpcc_threads
do  
        for wl in $workloads
        do
            for wh in $warehouse
            do
                think_time="tpcc"
                runTpccNTimes 
            done
        done
done

tpcc_threads="20 200 400 800 1200 1600"
workloads="3"
for t in $tpcc_threads
do  
        for wl in $workloads
        do
	    for len in $tpcc_length
	    do
            for wh in $warehouse
            do
		for len in $tpcc_length
		do
                think_time="tpcc"
                runTpccNTimes 
		done
            done
	    done
        done
done
fi

Folder="./specula_tests/macro/internal/rubis/"
rubis_threads="50 500 1000 2000 3000 4000 5000 6000"
for t in $rubis_threads
do  
        think_time="rubis"
        runRubis
done

exit

do_specula=true
specula_read=false
len=16

sudo ./masterScripts/initMachnines.sh 1 benchmark_no_specula_remove_stat 
sudo ./script/parallel_command.sh "cd antidote && sudo make rel"

Folder="./specula_tests/planet16"
rm -rf ./config
echo tpcc cdf true >> config
echo tpcc duration 120 >> config
echo rubis cdf true >> config
echo rubis duration 120 >> config
echo ant cdf true >> ./config
sudo ./script/copy_to_all.sh ./config ./basho_bench/
sudo ./script/parallel_command.sh "cd basho_bench && sudo ./script/config_by_file.sh"

sudo ./script/configBeforeRestart.sh 5000 $do_specula $len $rep $parts $specula_read 
sudo ./script/restartAndConnect.sh


tpcc_threads="10 100 200 400 600 800"
workloads="1"
for t in $tpcc_threads
do  
        for wl in $workloads
        do
            for wh in $warehouse
            do
                think_time="tpcc"
                runTpccNTimes 
            done
        done
done

tpcc_threads="20 200 400 600 800 1000"
workloads="2"
for t in $tpcc_threads
do  
        for wl in $workloads
        do
            for wh in $warehouse
            do
                think_time="tpcc"
                runTpccNTimes 
            done
        done
done

tpcc_threads="20 200 400 800 1200 1600"
workloads="3"
for t in $tpcc_threads
do  
        for wl in $workloads
        do
            for wh in $warehouse
            do
                think_time="tpcc"
                runTpccNTimes 
            done
        done
done

#Folder="./specula_tests/baseline/rubis"
rubis_threads="50 500 1000 2000 3000 4000"
for t in $rubis_threads
do  
        think_time="rubis"
        runRubis
done
exit


do_specula=true
specula_read=false
len=1

#sudo ./script/configBeforeRestart.sh 5000 $do_specula $len $rep $parts $specula_read 
#sudo ./script/restartAndConnect.sh

if [ 1 == 2 ];
then
tpcc_threads="100 10 200 400 600 800 1000"
workloads="1"
for t in $tpcc_threads
do  
        for wl in $workloads
        do
            for wh in $warehouse
            do
                think_time="tpcc"
                runTpccNTimes 
            done
        done
done

tpcc_threads="100 10 300 600 900 1200 1500"
workloads="2"
for t in $tpcc_threads
do  
        for wl in $workloads
        do
            for wh in $warehouse
            do
                think_time="tpcc"
                runTpccNTimes 
            done
        done
done

tpcc_threads="300 30 600 1200 1800 2400 3000"
workloads="3"
for t in $tpcc_threads
do  
        for wl in $workloads
        do
            for wh in $warehouse
            do
                think_time="tpcc"
                runTpccNTimes 
            done
        done
done
fi

rubis_threads="4000"
for t in $rubis_threads
do  
        think_time="rubis"
        runRubis
done
