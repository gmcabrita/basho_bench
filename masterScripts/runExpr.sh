#!/bin/bash
set -e

AllNodes=`cat ./scripts/allnodes` 

#Change config for basho_bench
for Node in ${AllNodes[@]}
do
  ./masterScripts/changeConfig.sh ./basho_bench/examples/tpcc.config antidote_pb_ips ['antidote@$Node']
  ./masterScripts/changeConfig.sh ./basho_bench/examples/load.config antidote_pb_ips ['antidote@$Node']
done

./masterScripts/changeConfig.sh ./basho_bench/examples/tpcc.config accessMaster 80
./masterScripts/changeConfig.sh ./basho_bench/examples/tpcc.config accessSlave 10
./masterScripts/changeConfig.sh ./antidote/rel/antidote/antidote.config do_repl true 
./masterScripts/changeConfig.sh ./antidote/rel/antidote/antidote.config do_specula true 

#Restart nodes and join
./scripts/parallelCommand.sh "sudo ./basho_bench ./examples/load.sh"
./scripts/parallelCommand.sh "sudo ./basho_bench ./examples/tpcc.sh"
