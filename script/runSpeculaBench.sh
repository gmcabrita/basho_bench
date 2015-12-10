#!/bin/bash
set -u
set -e
AllNodes=`cat script/allnodes`

if [ $# -ne 5 ]
then
	echo "Wrong usage: concurrent, accessMaster, accessSlave, do_specula, folder"
    exit
fi

#Params: nodes, cookie, num of dcs, num of nodes, if connect dcs, replication or not, branch
Time=`date +'%Y-%m-%d-%H:%M:%S'`
Folder=$5/$Time
mkdir $Folder
Tpcc="./basho_bench/examples/tpcc.config"
Load="./basho_bench/examples/load.config"
Ant="./antidote/rel/antidote/antidote.config"
./masterScripts/changeConfig.sh "$AllNodes" $Tpcc concurrent $1
./masterScripts/changeConfig.sh "$AllNodes" $Load concurrent 1
./masterScripts/changeConfig.sh "$AllNodes" $Tpcc to_sleep 10000 
./masterScripts/changeConfig.sh "$AllNodes" $Load to_sleep 10000
./masterScripts/changeConfig.sh "$AllNodes" $Tpcc accessMaster $2
./masterScripts/changeConfig.sh "$AllNodes" $Tpcc accessSlave $3
./masterScripts/changeConfig.sh "$AllNodes" $Ant do_specula $4
./masterScripts/changeConfig.sh "$AllNodes" $Ant do_repl true

./script/restartAndConnect.sh "$AllNodes"  antidote 
./script/parallel_command.sh "cd basho_bench && sudo mkdir -p tests && sudo ./basho_bench examples/load.config"
./script/parallel_command.sh "cd basho_bench && sudo mkdir -p tests && sudo ./basho_bench examples/tpcc.config"
for Node in $AllNodes
do
    Results=`./script/command_to_all.sh $Node "./basho_bench/localScripts/getStat.sh" | tail -1 `
    Results=`cut -d ">" -f 2 <<< "$Results"`
    Results=`cut -d "(" -f 1 <<< "$Results"`
    Results=(${Results//,/ })
    HitCache=${Results[0]}
    ReadAborted=${Results[1]}
    SpeculaAborted=${Results[2]}
    Committed=${Results[3]}
    SpeculaCommitted=${Results[4]}
    echo $Node ": Hit cache:" $HitCache ", ReadAborted:" $ReadAborted ", SpeculaAborted:" $SpeculaAborted", Committed:"$Committed", speculaCommitted:"$SpeculaCommitted >> $File/stat
done
./script/gatherThroughput.sh $Folder
