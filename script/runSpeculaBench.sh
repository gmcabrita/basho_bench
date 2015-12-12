#!/bin/bash
set -u
set -e
AllNodes=`cat script/allnodes`

if [ $# -ne 6 ]
then
	echo "Wrong usage: concurrent, accessMaster, accessSlave, do_specula, do_fast_reply, folder"
    exit
fi

#Params: nodes, cookie, num of dcs, num of nodes, if connect dcs, replication or not, branch
Time=`date +'%Y-%m-%d-%H:%M:%S'`
Folder=$6/$Time
mkdir $Folder
Tpcc="./basho_bench/examples/tpcc.config"
Load="./basho_bench/examples/load.config"
Ant="./antidote/rel/antidote/antidote.config"
./masterScripts/changeConfig.sh "$AllNodes" $Tpcc code_paths "[\x22\x2E\x2E\x2Fantidote\x2Febin\x22]" 
./masterScripts/changeConfig.sh "$AllNodes" $Load code_paths "[\x22\x2E\x2E\x2Fantidote\x2Febin\x22]"
./masterScripts/changeConfig.sh "$AllNodes" $Tpcc concurrent $1
./masterScripts/changeConfig.sh "$AllNodes" $Load concurrent 1
./masterScripts/changeConfig.sh "$AllNodes" $Tpcc to_sleep 5000 
./masterScripts/changeConfig.sh "$AllNodes" $Load to_sleep 5000
./masterScripts/changeConfig.sh "$AllNodes" $Tpcc accessMaster $2
./masterScripts/changeConfig.sh "$AllNodes" $Tpcc accessSlave $3
./masterScripts/changeConfig.sh "$AllNodes" $Ant do_specula $4
./masterScripts/changeConfig.sh "$AllNodes" $Ant do_repl true
./masterScripts/changeConfig.sh "$AllNodes" $Ant fast_reply $5

./script/restartAndConnect.sh "$AllNodes"  antidote 
./script/parallel_command.sh "cd basho_bench && sudo mkdir -p tests && sudo ./basho_bench examples/load.config"
./script/parallel_command.sh "cd basho_bench && sudo mkdir -p tests && sudo ./basho_bench examples/tpcc.config"
./script/gatherThroughput.sh $Folder
./script/parseStat.sh $Folder
