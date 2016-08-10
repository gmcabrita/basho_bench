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
elif [ $# == 12 ]
then
    think_time=$5
    WPerDc=$8
    new_order=$9
    payment=${10}
    order_status=$((100-${9}-${10}))
    repl_degree=${11}
    seq=${12}
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
sudo rm -f config
echo tpcc concurrent $1 >> config 
echo tpcc access_master $2  >> config
echo tpcc access_slave $3 >> config
echo load concurrent 4 >> config
#echo tpcc duration 60 >> config
echo tpcc think_time $think_time >> config
echo tpcc specula $4 >> config
echo tpcc operations "[{new_order,$new_order},{payment,$payment},{order_status,$order_status}]" >> config
#ToSleep=$((40000 / ${1}))
NumNodes=`cat ./script/allnodes | wc -l`
#MasterToSleep=$((NumNodes*400*WPerDc+4000))
#ToSleep=$(((18000 + 600*NumNodes) / ${1}))
#ToSleep=$(($ToSleep>200?$ToSleep:200))
#MasterToSleep=$((NumNodes*600+25000 - ${1}))
MasterToSleep=$((NumNodes*600+55000 - ${1}*8))
MasterToSleep=$((MasterToSleep<0?0:${MasterToSleep}))
echo tpcc master_to_sleep $MasterToSleep >> config
#echo tpcc to_sleep $ToSleep >> config
#echo load to_sleep 35000 >> config
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
touch $Folder/$seq
sudo ./script/parallel_command.sh "cd basho_bench && sudo ./script/config_by_file.sh"

./script/parallel_command.sh "cd basho_bench && sudo mkdir -p tests && sudo ./basho_bench examples/tpcc.config" &
#./script/parallel_command.sh "`cat ./script/allnodes | head -2`" "cd basho_bench && sudo mkdir -p tests && sudo ./basho_bench examples/tpcc.config" &
./script/clean_data.sh
./script/load.sh `head -1 ./script/allnodes` tpcc $WPerDc
wait

./script/gatherThroughput.sh $Folder &
./script/gatherLatency.sh $Folder &
wait

#sleep 5
#if [ $ToSleep -lt 20000 ]
#then
#sleep 15
#fi
#timeout 200 ./script/parallel_command.sh "cd basho_bench && sudo mkdir -p tests && sudo ./basho_bench examples/tpcc.config"
#./script/getAbortStat.sh `head -1 ./script/allnodes` $Folder 

#./script/fetchAndParseStat.sh $Folder

#sudo ./script/parallel_command.sh "cd basho_bench && sudo ./script/merge_latency.sh"
#./script/copyFromAll.sh latency_final ./antidote/rel/antidote/ $Folder
#./script/copyFromAll.sh latency_percv ./antidote/rel/antidote/ $Folder
#sudo pkill -P $$
#./script/verifySame.sh $Folder 
