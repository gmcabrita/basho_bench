#!/bin/bash
set -u
set -e
AllNodes=`cat script/allnodes`

if [ $# -ne 4 ]
then
	echo "Wrong usage: concurrent, accessMaster, accessSlave, do_specula"
    exit
fi

#Params: nodes, cookie, num of dcs, num of nodes, if connect dcs, replication or not, branch
Time=`date +'%Y-%m-%d-%H:%M:%S'`
Folder=$1/$Time
mkdir $Folder
Tpcc="./basho_bench/examples/tpcc.config"
Load="./basho_bench/examples/load.config"
Ant="./antidote/rel/antidote/antidote.config"
./masterScripts/changeConfig.sh "$AllNodes" $Tpcc concurrent $1
./masterScripts/changeConfig.sh "$AllNodes" $Tpcc accessMaster $2
./masterScripts/changeConfig.sh "$AllNodes" $Tpcc accessSlave $3
./masterScripts/changeConfig.sh "$AllNodes" $Ant do_specula $4
./masterScripts/changeConfig.sh "$AllNodes" $Ant do_repl true

./script/restartAndConnect.sh "$AllNodes"  antidote 
./script/parallel_command.sh "./basho_bench examples/load.config"
./script/parallel_command.sh "./basho_bench examples/tpcc.config"
for Node in $Nodes
do
    Result=`./script/command_to_all.sh $Node "./scripts/getStat.sh"`
    echo $Result >> $Folder/stat
done
./script/gatherThroughput.sh $Folder
