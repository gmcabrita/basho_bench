#!/bin/bash
set -u
set -e
AllNodes=`cat script/allnodes`

#Params: nodes, cookie, num of dcs, num of nodes, if connect dcs, replication or not, branch
#./script/parallel_command.sh "$AllNodes" "sudo service ntp stop"
#%./script/stablizeTime.sh &
#Pid=$!

./script/makeRel.sh simple_speculation
./script/changePartition.sh 4 
./script/changeReplication.sh true quorum 3 3 
sudo sed -i 's/^{operations.*/{operations, [{workload_by_id, 1}]}./' examples/antidote_pb.config 

sudo sed -i 's/^{num_updates.*/{num_updates, 4}./' examples/antidote_pb.config 
sudo sed -i 's/^{num_txns.*/{num_txns, 4}./' examples/antidote_pb.config 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 1 

exit

./script/changeReplication.sh true chain 3 3 
sudo sed -i 's/^{operations.*/{operations, [{general_tx, 1}]}./' examples/antidote_pb.config 

sudo sed -i 's/^{num_updates.*/{num_updates, 4}./' examples/antidote_pb.config 
sudo sed -i 's/^{num_txns.*/{num_txns, 4}./' examples/antidote_pb.config 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 1 

./script/makeRel.sh opt_speculation
./script/changePartition.sh 4 
./script/changeReplication.sh true quorum 3 3 
sudo sed -i 's/^{operations.*/{operations, [{general_tx, 1}]}./' examples/antidote_pb.config 

sudo sed -i 's/^{num_updates.*/{num_updates, 4}./' examples/antidote_pb.config 
sudo sed -i 's/^{num_txns.*/{num_txns, 4}./' examples/antidote_pb.config 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 1 

./script/changeReplication.sh true chain 3 3 
sudo sed -i 's/^{operations.*/{operations, [{general_tx, 1}]}./' examples/antidote_pb.config 

sudo sed -i 's/^{num_updates.*/{num_updates, 4}./' examples/antidote_pb.config 
sudo sed -i 's/^{num_txns.*/{num_txns, 4}./' examples/antidote_pb.config 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 1 

