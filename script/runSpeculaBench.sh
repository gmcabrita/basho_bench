#!/bin/bash
set -u
set -e
AllNodes=`cat script/allnodes`

if [ $# == 7 ]
then
    echo "Use default num for workload"
    WPerDc=1
    new_order=45
    payment=45
    order_status=10
elif [ $# == 8 ]
then
    WPerDc=$8
    new_order=45
    payment=45
    order_status=10
elif [ $# == 11 ]
then
    WPerDc=$8
    new_order=$9
    payment=${10}
    order_status=$((100-${9}-${10}))
    repl_degree=${11}
else
    echo "Wrong usage: concurrent, accessMaster, accessSlave, do_specula, fast_reply, specula_length, folder, [num_partitions]"
    exit
fi

echo "Params are" $1 $2 $3 $4 $5 $6 $WPerDc $9 ${10} 
#Params: nodes, cookie, num of dcs, num of nodes, if connect dcs, replication or not, branch
Time=`date +'%Y-%m-%d-%H%M%S'`
Folder=$7/$Time
echo "Folder to make is" $Folder
mkdir $Folder
Tpcc="./basho_bench/examples/tpcc.config"
Load="./basho_bench/examples/load.config"
Ant="./antidote/rel/antidote/antidote.config"
AppConfig="./antidote/rel/antidote/etc/app.config"
sudo rm -f config
echo tpcc concurrent $1 >> config 
echo ant concurrent $1 >> config 
echo tpcc access_master $2  >> config
echo tpcc access_slave $3 >> config
echo ant  do_specula $4  >> config
echo ant fast_reply $5   >> config
echo ant specula_length $6  >> config
echo load concurrent 4 >> config
echo tpcc duration 60 >> config
echo tpcc operations "[{new_order,$new_order},{payment,$payment},{order_status,$order_status}]" >> config
#ToSleep=$((40000 / ${1}))
NumNodes=`cat ./script/allnodes | wc -l`
MasterToSleep=$((NumNodes*1000+10000))
ToSleep=$(((10000 + 500*NumNodes) / ${1}))
echo tpcc master_to_sleep $MasterToSleep >> config
echo tpcc to_sleep $ToSleep >> config
#echo load to_sleep 35000 >> config
echo ant num_dcs  `cat ./script/num_dcs` >> config 
echo ant do_repl true >> config
echo app_config ring_creation_size 32 >> config
echo tpcc w_per_dc $WPerDc >> config
echo load w_per_dc $WPerDc >> config
if [ "$WPerDc" -eq 1 ]
then
    echo load duration 130 >> config
elif [ "$WPerDc" -eq 2 ]
then
    echo load duration 205 >> config
else
    echo load duration 280 >> config
fi

sudo ./script/copy_to_all.sh ./config ./basho_bench/
echo $1 $2 $3 $4 $5 $6 $WPerDc $9 ${10} ${11}  > $Folder/config
sudo ./script/parallel_command.sh "cd basho_bench && sudo ./script/config_by_file.sh && sudo ./script/configReplication.sh $repl_degree"

./script/restartAndConnect.sh
#Change Load params
#./masterScripts/changeConfig.sh "$AllNodes" $Tpcc duration 1 
#./masterScripts/changeConfig.sh "$AllNodes" $Load duration 1 
#./masterScripts/changeConfig.sh "$AllNodes" $Tpcc to_sleep 8000 
#./masterScripts/changeConfig.sh "$AllNodes" $Load to_sleep 7000
#./masterScripts/changeConfig.sh "$AllNodes" $Ant do_repl true

#./script/restartAndConnect.sh "$AllNodes"  antidote 
#./script/restartNodes.sh 
#sleep 20

#Time=`date +%s`
#./script/parallel_command.sh "cd basho_bench && sudo mkdir -p tests && sudo ./basho_bench examples/load.config"
#NewTime=`date +%s`
#Duration=$((NewTime-Time))
#if [ "$Duration" -lt 60 ]
#then
#echo "Load failed... Trying again!"
#sleep 5
#./script/parallel_command.sh "cd basho_bench && sudo mkdir -p tests && sudo ./basho_bench examples/load.config"
#fi

./script/parallel_command.sh "cd basho_bench && sudo mkdir -p tests && sudo ./basho_bench examples/tpcc.config" &
./script/load.sh `head -1 ./script/allnodes` tpcc $WPerDc
wait

#sleep 5
#if [ $ToSleep -lt 20000 ]
#then
#sleep 15
#fi
#timeout 200 ./script/parallel_command.sh "cd basho_bench && sudo mkdir -p tests && sudo ./basho_bench examples/tpcc.config"
./script/gatherThroughput.sh $Folder &
./script/copyFromAll.sh prep ./basho_bench/tests/current/ $Folder & 
./script/copyFromAll.sh new-order_latencies.csv ./basho_bench/tests/current/ $Folder & 
./script/copyFromAll.sh payment_latencies.csv ./basho_bench/tests/current/ $Folder & 
./script/copyFromAll.sh order-status_latencies.csv ./basho_bench/tests/current/ $Folder & 
wait
./script/getAbortStat.sh `head -1 ./script/allnodes` $Folder 

#for N in $AllNodes
#do
#./script/parseStat.sh $N $Folder
#done
./script/fetchAndParseStat.sh $Folder

sudo pkill -P $$
