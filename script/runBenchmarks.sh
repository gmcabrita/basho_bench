#!/bin/bash
set -u
set -e
AllNodes=`cat script/allnodes`

#Params: nodes, cookie, num of dcs, num of nodes, if connect dcs, replication or not, branch
./script/makeRel.sh simple_speculation 
File="./antidote/rel/antidote/antidote.config"
sudo sed -i 's/^{operations.*/{operations, [{general_tx, 1}]}./' examples/antidote_pb.config
sudo sed -i 's/^{num_updates.*/{num_updates, 3}./' examples/antidote_pb.config
sudo sed -i 's/^{op_type.*/{op_type, update}./' examples/antidote_pb.config
sudo sed -i 's/^{num_txns.*/{num_txns, 1}./' examples/antidote_pb.config
sudo sed -i 's/^{key_gen_mode.*/{key_gen_mode, 13}./' examples/antidote_pb.config
sudo sed -i 's/^{concurrent.*/{concurrent, 8}./' examples/antidote_pb.config

./script/changePartition.sh 4
./script/changeReplication.sh true chain 3 3 

./script/replaceConfig.sh delay 0  $File
./script/replaceConfig.sh specula_timeout 0 $File
./script/restartAndConnect.sh "$AllNodes"  antidote 1 4 
./basho_bench examples/antidote_pb.config &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

./script/replaceConfig.sh delay 0  $File
./script/replaceConfig.sh specula_timeout 1000 $File
./script/restartAndConnect.sh "$AllNodes"  antidote 1 4 
./basho_bench examples/antidote_pb.config &> /dev/null &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

./script/replaceConfig.sh delay 0  $File
./script/replaceConfig.sh specula_timeout 2000 $File
./script/restartAndConnect.sh "$AllNodes"  antidote 1 4 
./basho_bench examples/antidote_pb.config &> /dev/null &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

./script/replaceConfig.sh delay 0  $File
./script/replaceConfig.sh specula_timeout 4000 $File
./script/restartAndConnect.sh "$AllNodes"  antidote 1 4 
./basho_bench examples/antidote_pb.config &> /dev/null &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

./script/replaceConfig.sh delay 0  $File
./script/replaceConfig.sh specula_timeout 6000 $File
./script/restartAndConnect.sh "$AllNodes"  antidote 1 4 
./basho_bench examples/antidote_pb.config &> /dev/null &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

exit

./script/replaceConfig.sh delay 1  $File
./script/replaceConfig.sh specula_timeout 0 $File
./script/restartAndConnect.sh "$AllNodes"  antidote 1 4 
./basho_bench examples/antidote_pb.config &> /dev/null &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

./script/replaceConfig.sh delay 1  $File
./script/replaceConfig.sh specula_timeout 1000 $File
./script/restartAndConnect.sh "$AllNodes"  antidote 1 4 
./basho_bench examples/antidote_pb.config &> /dev/null &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

./script/replaceConfig.sh delay 1  $File
./script/replaceConfig.sh specula_timeout 2000 $File
./script/restartAndConnect.sh "$AllNodes"  antidote 1 4 
./basho_bench examples/antidote_pb.config &> /dev/null &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

./script/replaceConfig.sh delay 1  $File
./script/replaceConfig.sh specula_timeout 4000 $File
./script/restartAndConnect.sh "$AllNodes"  antidote 1 4 
./basho_bench examples/antidote_pb.config &> /dev/null &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

./script/replaceConfig.sh delay 1  $File
./script/replaceConfig.sh specula_timeout 6000 $File
./script/restartAndConnect.sh "$AllNodes"  antidote 1 4 
./basho_bench examples/antidote_pb.config &> /dev/null &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

./script/replaceConfig.sh delay 2  $File
./script/replaceConfig.sh specula_timeout 0 $File
./script/restartAndConnect.sh "$AllNodes"  antidote 1 4 
./basho_bench examples/antidote_pb.config &> /dev/null &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

./script/replaceConfig.sh delay 2  $File
./script/replaceConfig.sh specula_timeout 1000 $File
./script/restartAndConnect.sh "$AllNodes"  antidote 1 4 
./basho_bench examples/antidote_pb.config &> /dev/null &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

./script/replaceConfig.sh delay 2  $File
./script/replaceConfig.sh specula_timeout 2000 $File
./script/restartAndConnect.sh "$AllNodes"  antidote 1 4 
./basho_bench examples/antidote_pb.config &> /dev/null &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

./script/replaceConfig.sh delay 2  $File
./script/replaceConfig.sh specula_timeout 4000 $File
./script/restartAndConnect.sh "$AllNodes"  antidote 1 4 
./basho_bench examples/antidote_pb.config &> /dev/null &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh

./script/replaceConfig.sh delay 2  $File
./script/replaceConfig.sh specula_timeout 6000 $File
./script/restartAndConnect.sh "$AllNodes"  antidote 1 4 
./basho_bench examples/antidote_pb.config &> /dev/null &
cd ../basho_bench2 && ./basho_bench ./examples/antidote_pb.config &
for job in `jobs -p`
do  wait $job
done
./script/getStat.sh
