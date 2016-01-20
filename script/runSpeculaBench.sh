#!/bin/bash
set -u
set -e
AllNodes=`cat script/allnodes`

if [ $# == 7 ]
then
    echo "Use default num for district, item and customer"
    WPerDc=1
elif [ $# == 8 ]
then
    WPerDc=$8
else
    echo "Wrong usage: concurrent, accessMaster, accessSlave, do_specula, fast_reply, specula_length, folder, [num_partitions]"
    exit
fi

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
echo tpcc access_master $2  >> config
echo tpcc access_slave $3 >> config
echo ant  do_specula $4  >> config
echo ant fast_reply $5   >> config
echo ant specula_length $6  >> config
echo load concurrent 4 >> config
echo tpcc duration 60 >> config
ToSleep=$((15000 / ${1} +500))
echo tpcc to_sleep $ToSleep >> config
echo load to_sleep 15000 >> config
echo ant do_repl true >> config
echo app_config ring_creation_size 12 >> config
echo tpcc w_per_dc $WPerDc >> config
echo load w_per_dc $WPerDc >> config
if [ "$WPerDc" -eq 1 ]
then
    echo load duration 105 >> config
elif [ "$WPerDc" -eq 2 ]
then
    echo load duration 165 >> config
else
    echo load duration 250 >> config
fi

sudo ./script/copy_to_all.sh ./config ./basho_bench/
sudo ./script/parallel_command.sh "cd basho_bench && sudo ./script/config_by_file.sh"

#Change Load params
#./masterScripts/changeConfig.sh "$AllNodes" $Tpcc duration 1 
#./masterScripts/changeConfig.sh "$AllNodes" $Load duration 1 
#./masterScripts/changeConfig.sh "$AllNodes" $Tpcc to_sleep 8000 
#./masterScripts/changeConfig.sh "$AllNodes" $Load to_sleep 7000
#./masterScripts/changeConfig.sh "$AllNodes" $Ant do_repl true

./script/restartAndConnect.sh "$AllNodes"  antidote 
Time=`date +%s`
./script/parallel_command.sh "cd basho_bench && sudo mkdir -p tests && sudo ./basho_bench examples/load.config"
NewTime=`date +%s`
Duration=$((NewTime-Time))
if [ "$Duration" -lt 60 ]
then
echo "Load failed... Trying again!"
sleep 5
./script/parallel_command.sh "cd basho_bench && sudo mkdir -p tests && sudo ./basho_bench examples/load.config"
fi
./script/parallel_command.sh "cd basho_bench && sudo mkdir -p tests && sudo ./basho_bench examples/tpcc.config"
./script/gatherThroughput.sh $Folder &
./script/copyFromAll.sh prep ./basho_bench/tests/current/ $Folder & 
./script/copyFromAll.sh new-order_latencies.csv ./basho_bench/tests/current/ $Folder & 
./script/copyFromAll.sh payment_latencies.csv ./basho_bench/tests/current/ $Folder & 
wait
./script/getAbortStat.sh `head -1 ./script/allnodes` $Folder 

for N in $AllNodes
do
./script/parseStat.sh $N $Folder
done
for N in $AllNodes
do
cat $Folder/$N-stat >> $Folder/stat
rm $Folder/$N-stat
done

echo $1 $2 $3 $4 $5 $6 $WPerDc > $Folder/config
