#!/bin/bash
set -u
set -e
AllNodes=`cat script/allnodes`

#Params: nodes, cookie, num of dcs, num of nodes, if connect dcs, replication or not, branch
./script/makeRel.sh simple_speculation 
File="./antidote/rel/antidote/antidote.config"

./script/changePartition.sh 4
./script/changeReplication.sh true chain 3 3 

./script/replaceConfig.sh delay 0  $File
./script/replaceConfig.sh specula_timeout 0 $File
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 
./basho_bench examples/antidote_pb.config &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

./script/replaceConfig.sh delay 0  $File
./script/replaceConfig.sh specula_timeout 1000 $File
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 
./basho_bench examples/antidote_pb.config &> /dev/null &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

./script/replaceConfig.sh delay 0  $File
./script/replaceConfig.sh specula_timeout 2000 $File
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 
./basho_bench examples/antidote_pb.config &> /dev/null &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

./script/replaceConfig.sh delay 0  $File
./script/replaceConfig.sh specula_timeout 4000 $File
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 
./basho_bench examples/antidote_pb.config &> /dev/null &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done

./script/getStat.sh
./script/replaceConfig.sh delay 1  $File
./script/replaceConfig.sh specula_timeout 0 $File
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 
./basho_bench examples/antidote_pb.config &> /dev/null &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

./script/replaceConfig.sh delay 1  $File
./script/replaceConfig.sh specula_timeout 1000 $File
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 
./basho_bench examples/antidote_pb.config &> /dev/null &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

./script/replaceConfig.sh delay 1  $File
./script/replaceConfig.sh specula_timeout 2000 $File
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 
./basho_bench examples/antidote_pb.config &> /dev/null &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

./script/replaceConfig.sh delay 2  $File
./script/replaceConfig.sh specula_timeout 0 $File
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 
./basho_bench examples/antidote_pb.config &> /dev/null &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

./script/replaceConfig.sh delay 2  $File
./script/replaceConfig.sh specula_timeout 1000 $File
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 
./basho_bench examples/antidote_pb.config &> /dev/null &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

./script/replaceConfig.sh delay 2  $File
./script/replaceConfig.sh specula_timeout 2000 $File
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 
./basho_bench examples/antidote_pb.config &> /dev/null &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

./script/makeRel.sh opt_speculation 
./script/changePartition.sh 4
./script/changeReplication.sh true chain 3 3 

./script/replaceConfig.sh delay 0  $File
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 
./basho_bench examples/antidote_pb.config &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

./script/replaceConfig.sh delay 1 $File
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 
./basho_bench examples/antidote_pb.config &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

./script/replaceConfig.sh delay 2 $File
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 
./basho_bench examples/antidote_pb.config &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

exit



sudo sed -i 's/^{op_type.*/{op_type, read}./' examples/antidote_pb.config 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 4 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 8 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 16 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 32 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 64 


./script/makeRel.sh simple_speculation
sudo sed -i 's/^{operations.*/{operations, [{general_tx, 1}]}./' examples/antidote_pb.config 
sudo sed -i 's/^{num_updates.*/{num_updates, 4}./' examples/antidote_pb.config 
sudo sed -i 's/^{op_type.*/{op_type, update}./' examples/antidote_pb.config 
sudo sed -i 's/^{num_txns.*/{num_txns, 4}./' examples/antidote_pb.config 
./script/changePartition.sh 4 
./script/changeReplication.sh true quorum 3 3 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 4 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 8 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 16 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 32 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 64 

sudo sed -i 's/^{op_type.*/{op_type, read}./' examples/antidote_pb.config 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 2 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 4 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 8 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 16 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 32 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 64 
exit

./script/makeRel.sh opt_speculation
sudo sed -i 's/^{operations.*/{operations, [{general_tx, 1}]}./' examples/antidote_pb.config 
sudo sed -i 's/^{num_updates.*/{num_updates, 4}./' examples/antidote_pb.config 
sudo sed -i 's/^{op_type.*/{op_type, all}./' examples/antidote_pb.config 
sudo sed -i 's/^{num_txns.*/{num_txns, 4}./' examples/antidote_pb.config 
./script/changePartition.sh 8 
./script/changeReplication.sh true quorum 3 3 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 20 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 20

./script/makeRel.sh simple_speculation
sudo sed -i 's/^{operations.*/{operations, [{general_tx, 1}]}./' examples/antidote_pb.config 
sudo sed -i 's/^{num_updates.*/{num_updates, 4}./' examples/antidote_pb.config 
sudo sed -i 's/^{op_type.*/{op_type, all}./' examples/antidote_pb.config 
sudo sed -i 's/^{num_txns.*/{num_txns, 4}./' examples/antidote_pb.config 
./script/changePartition.sh 8 
./script/changeReplication.sh true quorum 3 3 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 20
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 4 0 20
exit

sudo sed -i 's/^{num_txns.*/{num_txns, 8}./' examples/antidote_pb.config 
./script/changeReplication.sh true quorum 3 3 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 1 0 2 

./script/makeRel.sh opt_speculation
sudo sed -i 's/^{operations.*/{operations, [{workload_by_id, 1}]}./' examples/antidote_pb.config 
sudo sed -i 's/^{num_updates.*/{num_updates, 4}./' examples/antidote_pb.config 
sudo sed -i 's/^{num_txns.*/{num_txns, 4}./' examples/antidote_pb.config 
./script/changePartition.sh 4 
./script/changeReplication.sh true quorum 3 3 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 1 0 2 

sudo sed -i 's/^{num_txns.*/{num_txns, 8}./' examples/antidote_pb.config 
./script/changeReplication.sh true quorum 3 3 
./script/multiDCBenchmark.sh "$AllNodes"  antidote 1 1 0 2 
