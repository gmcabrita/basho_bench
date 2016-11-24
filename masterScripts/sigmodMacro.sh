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

#rep=8
#parts=28
#rep=5
#parts=28
##rep=3
#parts=20
rep=1
parts=8

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

sudo ./masterScripts/initMachnines.sh 1 benchmark_precise_remove_stat_forward
sudo ./script/parallel_command.sh "cd antidote && sudo make rel"

rm -rf ./config
echo tpcc cdf true >> config
echo tpcc duration 200 >> config
echo rubis cdf true >> config
echo rubis duration 200 >> config
echo rubis all_update true >> config
echo ant cdf true >> ./config
sudo ./script/copy_to_all.sh ./config ./basho_bench/
sudo ./script/parallel_command.sh "cd basho_bench && sudo ./script/config_by_file.sh"

sudo ./script/configBeforeRestart.sh 2000 $do_specula $len $rep $parts $specula_read
sudo ./script/restartAndConnect.sh

#tpcc_threads="200 400 600 800 1000"
tpcc_threads="800"
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

#tpcc_threads="300 600 900 1200 1500"
#tpcc_threads="900 1200"
tpcc_threads="1200"
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

#tpcc_threads="600 1200 1800 2400 3000"
#tpcc_threads="1200 1800"
tpcc_threads="1800"
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

#rubis_threads="3000 4000 5000"
rubis_threads="4000"
for t in $rubis_threads
do  
        think_time="rubis"
        runRubis
done
exit


do_specula=false
specula_read=false
len=0

#sudo ./masterScripts/initMachnines.sh 1 benchmark_no_specula_remove_stat 
#sudo ./script/parallel_command.sh "cd antidote && sudo make rel"

rm -rf ./config
echo tpcc cdf true >> config
echo tpcc duration 120 >> config
echo rubis cdf true >> config
echo rubis duration 120 >> config
echo ant cdf true >> ./config
sudo ./script/copy_to_all.sh ./config ./basho_bench/
sudo ./script/parallel_command.sh "cd basho_bench && sudo ./script/config_by_file.sh"

#sudo ./script/configBeforeRestart.sh 1000 $do_specula 0 $rep $parts $specula_read 
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

rubis_threads="500 50 1000 2000 3000 4000 5000"
for t in $rubis_threads
do  
        think_time="rubis"
        runRubis
done
fi


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
