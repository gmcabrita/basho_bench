#!/bin/bash

RepDegree=$1
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
I=0
Length=${#AntNodeArray[@]}
echo $Length
for Node in $AllNodes
do
    CurrentNode="'antidote@"$Node"'"
    NextI=$(((I+1) % Length))
    DNextI=$(((I+2) % Length))
    ThirdI=$(((I+3) % Length))
    FourthI=$(((I+4) % Length))
    if [ $RepDegree -eq 2 ]; then
    	if [ $I -ne 0 ]; then
            ReplList=$ReplList",{"$CurrentNode",["${AntNodeArray[$NextI]}","${AntNodeArray[$DNextI]}"]}"
    	else
            ReplList=$ReplList"{"$CurrentNode",["${AntNodeArray[$NextI]}","${AntNodeArray[$DNextI]}"]}"
    	fi
    elif [ $RepDegree -eq 3 ]; then
    	if [ $I -ne 0 ]; then
            ReplList=$ReplList",{"$CurrentNode",["${AntNodeArray[$NextI]}","${AntNodeArray[$DNextI]}","${AntNodeArray[$ThirdI]}"]}"
    	else
            ReplList=$ReplList"{"$CurrentNode",["${AntNodeArray[$NextI]}","${AntNodeArray[$DNextI]}","${AntNodeArray[$ThirdI]}"]}"
    	fi
    elif [ $RepDegree -eq 4 ]; then
	if [ $I -ne 0 ]; then
            ReplList=$ReplList",{"$CurrentNode",["${AntNodeArray[$NextI]}","${AntNodeArray[$DNextI]}","${AntNodeArray[$ThirdI]}","${AntNodeArray[$FourthI]}"]}"
        else
            ReplList=$ReplList"{"$CurrentNode",["${AntNodeArray[$NextI]}","${AntNodeArray[$DNextI]}","${AntNodeArray[$ThirdI]}","${AntNodeArray[$FourthI]}"]}"
        fi
    fi
    #if [ $I -ne 0 ]; then
    #    ReplList=$ReplList",{"$CurrentNode",["${AntNodeArray[$NextI]}","${AntNodeArray[$DNextI]}","${AntNodeArray[$ThirdI]}"]}"
    #else
    #    ReplList=$ReplList"{"$CurrentNode",["${AntNodeArray[$NextI]}","${AntNodeArray[$DNextI]}","${AntNodeArray[$ThirdI]}"]}"
    #fi
    I=$((I+1))
done
ReplList=$ReplList"]"
echo "$ReplList"
./localScripts/changeConfig.sh ../antidote/rel/antidote/antidote.config to_repl "$ReplList"
