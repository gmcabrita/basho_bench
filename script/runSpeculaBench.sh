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
MasterToSleep=$((NumNodes*400*WPerDc+14000))
ToSleep=$(((8000 + 500*NumNodes) / ${1}))
echo tpcc master_to_sleep $MasterToSleep >> config
echo tpcc to_sleep $ToSleep >> config
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

sleep 10 && ./script/parallel_command.sh "cd basho_bench && sudo mkdir -p tests && sudo ./basho_bench examples/tpcc.config" &
#./script/parallel_command.sh "`cat ./script/allnodes | head -2`" "cd basho_bench && sudo mkdir -p tests && sudo ./basho_bench examples/tpcc.config" &
./script/clean_data.sh
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
./script/copyFromAll.sh txn_latencies.csv ./basho_bench/tests/current/ $Folder & 
wait
#./script/getAbortStat.sh `head -1 ./script/allnodes` $Folder 

./script/fetchAndParseStat.sh $Folder
#timeout 60 ./script/fetchAndParseStat.sh $Folder
#if [ $? -eq 124 ]; then
#    timeout 60 ./script/fetchAndParseStat.sh $Folder
#    if [ $? -eq 124 ]; then
#        timeout 60 ./script/fetchAndParseStat.sh $Folder
#    fi
#fi

sudo pkill -P $$
./script/verifySame.sh $Folder 
