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

#rep=8
#parts=28
##rep=3
#parts=20
#rep=1
#parts=12
rep=1
parts=4

start_ind=1
skip_len=0
skipped=1
AM=80
AS=0

tpcc_length="0"
rubis_length="0"

do_specula=false
specula_read=false
len=0

if [ 1 == 2 ];
then
#sudo ./masterScripts/initMachnines.sh 1 benchmark_no_specula_remove_stat 

rm -rf ./config
echo tpcc cdf true >> config
echo tpcc duration 120 >> config
echo rubis cdf true >> config
echo rubis duration 120 >> config
echo ant cdf true >> ./config
sudo ./script/copy_to_all.sh ./config ./basho_bench/
sudo ./script/parallel_command.sh "cd basho_bench && sudo ./script/config_by_file.sh"

sudo ./script/configBeforeRestart.sh 10 $do_specula 0 $rep $parts $specula_read 
sudo ./script/restartAndConnect.sh

tpcc_threads="1500 2000"
workloads="1"
for t in $tpcc_threads
do  
        for wl in $workloads
        do
	        if [ $wl == 1 ]; then  n=80  p=10
	        elif [ $wl == 2 ]; then  n=10 p=80
	        elif [ $wl == 3 ]; then n=10 p=10
	        fi
            for wh in $warehouse
            do
                think_time="tpcc"
                runTpccNTimes 
            done
        done
done

tpcc_threads="800 1000"
workloads="2"
for t in $tpcc_threads
do  
        for wl in $workloads
        do
	        if [ $wl == 1 ]; then  n=80  p=10
	        elif [ $wl == 2 ]; then  n=10 p=80
	        elif [ $wl == 3 ]; then n=10 p=10
	        fi
            for wh in $warehouse
            do
                think_time="tpcc"
                runTpccNTimes 
            done
        done
done

tpcc_threads="1000 1500"
workloads="3"
for t in $tpcc_threads
do  
        for wl in $workloads
        do
	        if [ $wl == 1 ]; then  n=80  p=10
	        elif [ $wl == 2 ]; then  n=10 p=80
	        elif [ $wl == 3 ]; then n=10 p=10
	        fi
            for wh in $warehouse
            do
                think_time="tpcc"
                runTpccNTimes 
            done
        done
done

rubis_threads="3000 4000"
for t in $rubis_threads
do  
        think_time="rubis"
        runRubis
done
fi

specula_read=true
do_specula=true

tpcc_length="0"
rubis_length="0"
len=0

#sudo ./masterScripts/initMachnines.sh 1 benchmark_precise_remove_stat 

rm -rf ./config
echo tpcc cdf true >> config
echo tpcc duration 120 >> config
echo rubis cdf true >> config
echo rubis duration 120 >> config
echo rubis all_update true >> config
echo ant cdf true >> ./config
sudo ./script/copy_to_all.sh ./config ./basho_bench/
sudo ./script/parallel_command.sh "cd basho_bench && sudo ./script/config_by_file.sh"

#sudo ./script/configBeforeRestart.sh 2000 $do_specula $len $rep $parts $specula_read
#sudo ./script/restartAndConnect.sh

if [ 1 == 2 ];
then
#tpcc_threads="1500 2000"
tpcc_threads=""
workloads="1"
for t in $tpcc_threads
do  
        for wl in $workloads
        do
	        if [ $wl == 1 ]; then  n=80  p=10
	        elif [ $wl == 2 ]; then  n=10 p=80
	        elif [ $wl == 3 ]; then n=10 p=10
	        fi
            for wh in $warehouse
            do
                think_time="tpcc"
                runTpccNTimes 
            done
        done
done

#tpcc_threads="800 1000"
tpcc_threads=""
workloads="2"
for t in $tpcc_threads
do  
        for wl in $workloads
        do
	        if [ $wl == 1 ]; then  n=80  p=10
	        elif [ $wl == 2 ]; then  n=10 p=80
	        elif [ $wl == 3 ]; then n=10 p=10
	        fi
            for wh in $warehouse
            do
                think_time="tpcc"
                runTpccNTimes 
            done
        done
done

tpcc_threads="1000 1500"
workloads="3"
for t in $tpcc_threads
do  
        for wl in $workloads
        do
	        if [ $wl == 1 ]; then  n=80  p=10
	        elif [ $wl == 2 ]; then  n=10 p=80
	        elif [ $wl == 3 ]; then n=10 p=10
	        fi
	    for len in $tpcc_length
	    do
            for wh in $warehouse
            do
                think_time="tpcc"
                runTpccNTimes 
            done
	    done
        done
done
fi


for len in $rubis_length
do
#rubis_threads="500 1000 2000 3000 4000"
rubis_threads="2000 3000 4000"
for t in $rubis_threads
do  
        think_time="rubis"
        runRubis
done
done
