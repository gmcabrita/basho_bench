#!/bin/bash
set -u
set -e
AllNodes=`cat script/allnodes`

echo "Params are" $1 $2 $3 $4 $5 $6 $8 
#Params: nodes, cookie, num of dcs, num of nodes, if connect dcs, replication or not, branch
Time=`date +'%Y-%m-%d-%H%M%S'`
Folder=$7/$Time
echo "Folder to make is" $Folder
mkdir $Folder
sudo rm -f config
echo rubis concurrent $1 >> config 
echo rubis access_master $2  >> config
echo rubis access_slave $3 >> config
echo load concurrent 4 >> config
echo rubis duration 60 >> config
echo rubis specula $4 >> config
#ToSleep=$((40000 / ${1}))
NumNodes=`cat ./script/allnodes | wc -l`
MasterToSleep=$((NumNodes*400+14000))
ToSleep=$(((8000 + 500*NumNodes) / ${1}))
echo rubis master_to_sleep $MasterToSleep >> config
echo rubis to_sleep $ToSleep >> config
echo load duration 130 >> config

sudo ./script/copy_to_all.sh ./config ./basho_bench/
echo $1 $2 $3 $4 $5 $6 $8 > $Folder/config
touch $Folder/$seq
sudo ./script/parallel_command.sh "cd basho_bench && sudo ./script/config_by_file.sh"

./script/parallel_command.sh "cd basho_bench && sudo mkdir -p tests && sudo ./basho_bench examples/rubis.config" &
#./script/parallel_command.sh "`cat ./script/allnodes | head -2`" "cd basho_bench && sudo mkdir -p tests && sudo ./basho_bench examples/tpcc.config" &
./script/clean_data.sh
./script/load.sh `head -1 ./script/allnodes` rubis haha
wait


#sleep 5
#if [ $ToSleep -lt 20000 ]
#then
#sleep 15
#fi
#timeout 200 ./script/parallel_command.sh "cd basho_bench && sudo mkdir -p tests && sudo ./basho_bench examples/tpcc.config"
./script/gatherThroughput.sh $Folder &
#./script/copyFromAll.sh prep ./basho_bench/tests/current/ $Folder & 
#./script/copyFromAll.sh new-order_latencies.csv ./basho_bench/tests/current/ $Folder & 
#./script/copyFromAll.sh payment_latencies.csv ./basho_bench/tests/current/ $Folder & 
#./script/copyFromAll.sh order-status_latencies.csv ./basho_bench/tests/current/ $Folder & 
#./script/copyFromAll.sh txn_latencies.csv ./basho_bench/tests/current/ $Folder & 
wait
#./script/getAbortStat.sh `head -1 ./script/allnodes` $Folder 

timeout 60 ./script/fetchAndParseStat.sh $Folder
if [ $? -eq 124 ]; then
    timeout 60 ./script/fetchAndParseStat.sh $Folder
    if [ $? -eq 124 ]; then
        timeout 60 ./script/fetchAndParseStat.sh $Folder
    fi
fi

sudo pkill -P $$
./script/verifySame.sh $Folder 
