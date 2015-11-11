#!/bin/bash
set -e
Folder="speculaResults/"
File=$Folder`date +"%Y-%m-%d-%H:%M:%S"`
cd ../li_antidote
sudo ./restartAndConnect.sh
sleep 30
cd -
sudo ./basho_bench ./examples/antidote_pb.config
Aborted=`cat ./tests/current/summary.csv | awk -F',' '{print $5}' | tail -n +2 | awk '{s+=$1} END {print s}'`
SpeculaCommitted=`cat ./tests/current/summary.csv | awk -F',' '{print $4}' | tail -n +2 | awk '{s+=$1} END {print s}'`
Results=`./localScripts/getStat.sh | tail -1 `
Results=`cut -d ">" -f 2 <<< "$Results"`
Results=`cut -d "(" -f 1 <<< "$Results"`
Results=(${Results//,/ })
HitCache=${Results[0]}
ReadAborted=${Results[1]}
SpeculaAborted=${Results[2]}
Committed=${Results[3]}
echo "Hit cache:" $HitCache ", ReadAborted:" $ReadAborted ", SpeculaAborted:" $SpeculaAborted", Committed:"$Committed", speculaCommitted:"$SpeculaCommitted", Aborted:"$Aborted >> $File
