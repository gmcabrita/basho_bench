#!/bin/bash
set -e

AllNodes=`cat ./script/allnodes` 

./script/command_to_all.sh "$AllNodes" "cd basho_bench && git stash && git pull"

#Change config for basho_bench
ReplList="["
AntNodeArray=()
I=0
for Node in $AllNodes
do
    CurrentNode="'antidote@"$Node"'"
    AntNodeArray[$I]=$CurrentNode
    I=$((I+1))
done

I=0
Length=${#AntNodeArray[@]}
echo $Length
for Node in $AllNodes
do
    CurrentNode="'antidote@"$Node"'"
    ./masterScripts/changeConfig.sh $Node ./basho_bench/examples/tpcc.config antidote_pb_ips [$CurrentNode]
    ./masterScripts/changeConfig.sh $Node ./basho_bench/examples/load.config antidote_pb_ips [$CurrentNode]
    NextI=$(((I+1) % Length))
    DNextI=$(((I+2) % Length))
    if [ $I -ne 0 ]; then
        ReplList=$ReplList",{"$CurrentNode", ["${AntNodeArray[$NextI]}", "${AntNodeArray[$DNextI]}"]}"
    else
        ReplList=$ReplList"{"$CurrentNode", ["${AntNodeArray[$NextI]}", "${AntNodeArray[$DNextI]}"]}"
    fi
    I=$((I+1))
done
ReplList=$ReplList"]"
echo "$ReplList"
./masterScripts/changeConfig.sh "$AllNodes" ./antidote/rel/antidote/antidote.config to_repl $Repllist 
