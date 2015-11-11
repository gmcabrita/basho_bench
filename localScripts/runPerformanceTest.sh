#!/bin/bash
set -e
NumThreads=$1
NumBytes=$2
File=$3

sudo ./localScripts/changeForAllDevs.sh bytes $NumBytes 
sudo ./localScripts/changeConfig.sh ./examples/antidote_pb.config  concurrent $NumThreads 
sudo ./localScripts/changeConfig.sh ../basho_bench2/examples/antidote_pb.config  concurrent $NumThreads 

cd ../li_antidote
sudo ./restartAndConnect.sh
sleep 30
cd -
sudo ./basho_bench ./examples/antidote_pb.config
Committed1=`cat ./tests/current/summary.csv | awk -F',' '{print $4}' | tail -n +2 | awk '{s+=$1} END {print s}'`
AvgThroughput1=`expr $Committed1 / 120`
cd ../basho_bench2
sudo ./basho_bench ./examples/antidote_pb.config
Committed2=`cat ./tests/current/summary.csv | awk -F',' '{print $4}' | tail -n +2 | awk '{s+=$1} END {print s}'`
AvgThroughput2=`expr $Committed2 / 120`
cd -
Ratio=`expr $Committed1 / $Committed2`
echo "Threads:" $NumThreads " Bytes:"$NumBytes $AvgThroughput1 $AvgThroughput2 $Ratio >> $3

