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
MasterToSleep=$((NumNodes*400+10000))
ToSleep=$(((8000 + 400*NumNodes) / ${1}))
echo rubis master_to_sleep $MasterToSleep >> config
echo rubis to_sleep $ToSleep >> config
echo load duration 130 >> config

sudo ./script/copy_to_all.sh ./config ./basho_bench/
echo $1 $2 $3 $4 $5 $6 $8 > $Folder/config
touch $Folder/$8
sudo ./script/parallel_command.sh "cd basho_bench && sudo ./script/config_by_file.sh"

./script/parallel_command.sh "cd basho_bench && sudo mkdir -p tests && sudo ./basho_bench examples/rubis.config" &
./script/clean_data.sh
./script/load.sh `head -1 ./script/allnodes` rubis 1 
wait

./script/gatherThroughput.sh $Folder

./script/copyFromAll.sh prep ./basho_bench/tests/current/ $Folder
#./script/copyFromAll.sh register-user_latencies.csv ./basho_bench/tests/current/ $Folder &
#./script/copyFromAll.sh register_item.csv ./basho_bench/tests/current/ $Folder &
#./script/copyFromAll.sh store-bid_latencies.csv ./basho_bench/tests/current/ $Folder &
#./script/copyFromAll.sh store-buy-now_latencies.csv ./basho_bench/tests/current/ $Folder &
#./script/copyFromAll.sh store-comment_latencies.csv ./basho_bench/tests/current/ $Folder &
#wait
#./script/copyFromAll.sh prep ./basho_bench/tests/current/ $Folder & 
#./script/copyFromAll.sh new-order_latencies.csv ./basho_bench/tests/current/ $Folder & 
#./script/copyFromAll.sh payment_latencies.csv ./basho_bench/tests/current/ $Folder & 
#./script/copyFromAll.sh order-status_latencies.csv ./basho_bench/tests/current/ $Folder & 
#./script/copyFromAll.sh txn_latencies.csv ./basho_bench/tests/current/ $Folder & 
#./script/getAbortStat.sh `head -1 ./script/allnodes` $Folder 

gtimeout 60 ./script/fetchAndParseStat.sh $Folder
if [ $? -eq 124 ]; then
    gtimeout 60 ./script/fetchAndParseStat.sh $Folder
    if [ $? -eq 124 ]; then
        gtimeout 60 ./script/fetchAndParseStat.sh $Folder
    fi
fi

./script/verifySame.sh $Folder 
