#!/bin/bash
set -e

cd basho_bench
./script/command_to_all.sh "$AllNodes" "cd basho_bench && git stash && git pull"
AllNodes=`cat ./script/allnodes` 


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
Ip=`hostname --ip-address`
CurrentNode="'antidote@"$Ip"'"
./localScripts/changeConfig.sh examples/tpcc.config antidote_pb_ips [$CurrentNode]
./localScripts/changeConfig.sh examples/load.config antidote_pb_ips [$CurrentNode]

I=0
Length=${#AntNodeArray[@]}
echo $Length
for Node in $AllNodes
do
    CurrentNode="'antidote@"$Node"'"
    NextI=$(((I+1) % Length))
    DNextI=$(((I+2) % Length))
    if [ $I -ne 0 ]; then
        ReplList=$ReplList",{"$CurrentNode",["${AntNodeArray[$NextI]}","${AntNodeArray[$DNextI]}"]}"
    else
        ReplList=$ReplList"{"$CurrentNode",["${AntNodeArray[$NextI]}","${AntNodeArray[$DNextI]}"]}"
    fi
    I=$((I+1))
done
ReplList=$ReplList"]"
echo "$ReplList"
./localScripts/changeConfig.sh ../antidote/rel/antidote/antidote.config to_repl "$ReplList"
