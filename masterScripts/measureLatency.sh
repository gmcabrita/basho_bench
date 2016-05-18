#!/bin/bash

PingDegree=8
ReplDegree=6
AllNodes=`cat ./script/allnodes`

NumDcs=`cat ./script/num_dcs`
NumNodes=`cat ./script/allnodes | wc -l`
NodesPerDc=$((NumNodes / NumDcs))

#Change config for basho_bench
AntNodeArray=()
I=0
Ip=`GET http://169.254.169.254/2014-11-05/meta-data/public-ipv4`
MyId=0

for Node in $AllNodes
do
    if [ "$Node" = "$Ip" ]; then
        MyId=$I
    fi
    AntNodeArray[$I]=$Node
    I=$((I+1))
done

I=0
if [ "$NumDcs" -eq 1 ]; then
    Leap=1
else
    Leap=$NodesPerDc
fi

Length=${#AntNodeArray[@]}
sudo rm *to*
sudo rm *summary*
for NodeId in $(seq 1 $PingDegree);
do
        NextI=$(((MyId+NodeId*Leap) % Length))
        NodeToPing=${AntNodeArray[$NextI]}
        FileName=$Ip"to"$NodeId"-"$NodeToPing
        sudo rm $FileName
        ping -c 300 $NodeToPing > $FileName & 
done

wait
Summary=$MyId"summary-"$Ip
sudo rm -r $Summary
for NodeId in $(seq 1 $PingDegree);
do
    NextI=$(((MyId+NodeId*Leap) % Length))
    NodeToPing=${AntNodeArray[$NextI]}
    FileName=$Ip"to"$NodeId"-"$NodeToPing
    tail -1 $FileName | awk -F "= " '{print $2}' | awk -F "/" '{print $2}' >> $Summary 
done


