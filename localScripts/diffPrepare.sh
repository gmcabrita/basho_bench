#!/bin/bash
#set -e
NumThreads=$1
File=$2
DoSpecula=$3
KeyRange=$4

#sudo ./localScripts/changeForAllDevs.sh bytes $NumBytes 
sudo ./localScripts/changeConfig.sh ./examples/antidote_pb.config  concurrent $NumThreads 
sudo ./localScripts/changeConfig.sh ./examples/antidote_pb.config  key_range $KeyRange 
#sudo ./localScripts/changeConfig.sh ../basho_bench2/examples/antidote_pb.config  concurrent $NumThreads 

cd ../li_antidote
sudo ./restartAndConnect.sh $DoSpecula
sleep 30
cd -
sudo ./basho_bench ./examples/antidote_pb.config
Committed1=`cat ./tests/current/summary.csv | awk -F',' '{print $4}' | tail -n +2 | awk '{s+=$1} END {print s}'`
Aborted1=`cat ./tests/current/summary.csv | awk -F',' '{print $5}' | tail -n +2 | awk '{s+=$1} END {print s}'`

cd ../li_antidote2
sudo ./restartAndConnect.sh $DoSpecula
sleep 30
cd -
sudo ./basho_bench ./examples/antidote_pb.config
echo "Expr finished."
Committed2=`cat ./tests/current/summary.csv | awk -F',' '{print $4}' | tail -n +2 | awk '{s+=$1} END {print s}'`
Aborted2=`cat ./tests/current/summary.csv | awk -F',' '{print $5}' | tail -n +2 | awk '{s+=$1} END {print s}'`
Ratio=`expr $Committed1 / $Committed2`
echo "Before outputing"
echo "Threads:" $NumThreads $Committed1 $Committed2 $Ratio $Aborted1 $Aborted2 >> $File
echo "Finish outputing"

