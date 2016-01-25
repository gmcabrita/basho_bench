#!/bin/bash

RepDegree=$1
AllNodes=`cat ./script/allnodes`

NumDcs=`cat ./script/num_dcs`
NumNodes=`cat ./script/allnodes | wc -l`
NodesPerDc=$((NumNodes / NumDcs))
echo "Num of total nodes are "$NumNodes", num of dcs are "$NumDcs", node per dc is "$NodesPerDc

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
if [ "$NumDcs" -eq 1 ]; then
    Leap=1
else
    Leap=$NodesPerDc
fi
echo $Leap "leap is"

Length=${#AntNodeArray[@]}
echo $Length
for Node in $AllNodes
do
    CurrentNode="'antidote@"$Node"'"
    if [ $I -ne 0 ]; then
            ReplList=$ReplList",{"$CurrentNode
        else
            ReplList=$ReplList"{"$CurrentNode
    fi
    for NodeId in $(seq 1 $RepDegree);
    do
            NextI=$(((I+NodeId*Leap) % Length))
            echo "I is"$I", Next i is "$NextI", NodeId is"$NodeId
            ReplList=$ReplList",["${AntNodeArray[$NextI]}
    done
    ReplList=$ReplList"]}"
    I=$((I+1))
done
ReplList=$ReplList"]"
echo "$ReplList"
#./localScripts/changeConfig.sh ../antidote/rel/antidote/antidote.config to_repl "$ReplList"
