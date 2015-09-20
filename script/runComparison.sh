#!/bin/bash
set -u
set -e
AllNodes=`cat script/allnodes`

#Params: nodes, cookie, num of dcs, num of nodes, if connect dcs, replication or not, branch
./script/makeRel.sh simple_speculation

File="./antidote/rel/antidote/antidote.config"
./script/replaceConfig.sh delay 0  $File
./script/replaceConfig.sh specula_timeout 6000 $File

sudo sed -i 's/^{operations.*/{operations, [{general_tx, 1}]}./' examples/antidote_pb.config 
sudo sed -i 's/^{num_updates.*/{num_updates, 4}./' examples/antidote_pb.config 
sudo sed -i 's/^{op_type.*/{op_type, all}./' examples/antidote_pb.config 
sudo sed -i 's/^{num_txns.*/{num_txns, 4}./' examples/antidote_pb.config 
sudo sed -i 's/^{key_gen_mode.*/{key_gen_mode, random}./' examples/antidote_pb.config 
./script/changePartition.sh 4 
./script/changeReplication.sh true quorum 3 3 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 4 
./script/getStat.sh

exit

./script/makeRel.sh opt_speculation
./script/replaceConfig.sh delay 0  $File
sudo sed -i 's/^{operations.*/{operations, [{general_tx, 1}]}./' examples/antidote_pb.config 
sudo sed -i 's/^{num_updates.*/{num_updates, 4}./' examples/antidote_pb.config 
sudo sed -i 's/^{op_type.*/{op_type, all}./' examples/antidote_pb.config 
sudo sed -i 's/^{num_txns.*/{num_txns, 4}./' examples/antidote_pb.config 
sudo sed -i 's/^{key_gen_mode.*/{key_gen_mode, by_id}./' examples/antidote_pb.config 
./script/changePartition.sh 4 
./script/changeReplication.sh true quorum 3 3 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 
./script/getStat.sh
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 4 
./script/getStat.sh
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 8 
./script/getStat.sh
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 16 
./script/getStat.sh
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 32 
./script/getStat.sh
