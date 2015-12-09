#!/bin/bash
set -u
set -e
AllNodes=`cat script/allnodes`

if [ $@ -ne 4 ]
	echo "Wrong usage"

#Params: nodes, cookie, num of dcs, num of nodes, if connect dcs, replication or not, branch
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
./script/getStat.sh

