#!/bin/bash
set -u
set -e
AllNodes=`cat script/allnodes`

if [ $# == 11 ]
then
    concurrent=$1
    master_num=$2
    slave_num=$3
    cache_num=$4
    master_range=$5
    slave_range=$6
    cache_range=$7
    do_spedula=$8
    specula_length=$9
    pattern=${10}
    repl_degree=${11}
    folder=${12}
else
    echo "Wrong usage: concurrent, master_num, slave_num, cache_num, master_range, slave_range, cache_range, do_specula, fast_reply, specula_length, pattern, repl_degree, folder"
    exit
fi

#Params: nodes, cookie, num of dcs, num of nodes, if connect dcs, replication or not, branch
Time=`date +'%Y-%m-%d-%H%M%S'`
Folder=$7/$Time
echo "Folder to make is" $Folder
mkdir $Folder
echo $1 $2 $3 $4 $5 $6 $7 $8 $9 ${11}  > $Folder/config
sudo rm -f config
echo ant concurrent $1 >> config 
echo micro concurrent $1 >> config 
echo micro master_num $master_num  >> config
echo micro slave_num $slave_num >> config
echo micro cache_num $cache_num >> config
echo micro master_range $master_range >> config
echo micro slave_range $slave_range >> config
echo micro cache_range $cache_range >> config
echo micro pattern $pattern >> config
echo ant  do_specula $do_specula  >> config
echo ant fast_reply $fast_reply   >> config
echo ant specula_length $specula_length  >> config
echo tpcc duration 60 >> config
#ToSleep=$((40000 / ${1}))
NumNodes=`cat ./script/allnodes | wc -l`
MasterToSleep=$((NumNodes*1000+10000))
ToSleep=$(((10000 + 500*NumNodes) / ${1}))
echo micro master_to_sleep $MasterToSleep >> config
echo micro to_sleep $ToSleep >> config
#echo load to_sleep 35000 >> config
echo ant num_dcs  `cat ./script/num_dcs` >> config 
echo ant do_repl true >> config
echo app_config ring_creation_size 32 >> config

sudo ./script/copy_to_all.sh ./config ./basho_bench/
sudo ./script/parallel_command.sh "cd basho_bench && sudo ./script/config_by_file.sh && sudo ./script/configReplication.sh $repl_degree"

./script/clean_data.sh

./script/parallel_command.sh "cd basho_bench && sudo mkdir -p tests && sudo ./basho_bench examples/micro.config" &
./script/load.sh `head -1 ./script/allnodes` micro 500000 
wait

./script/gatherThroughput.sh $Folder &
./script/copyFromAll.sh prep ./basho_bench/tests/current/ $Folder & 
./script/copyFromAll.sh txn_latencies.csv ./basho_bench/tests/current/ $Folder & 
wait
./script/getAbortStat.sh `head -1 ./script/allnodes` $Folder 

#for N in $AllNodes
#do
#./script/parseStat.sh $N $Folder
#done
./script/fetchAndParseStat.sh $Folder

sudo pkill -P $$
