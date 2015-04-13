#!/bin/bash

if [ $# -eq 0 ]; then
	echo "Usage: all_nodes, cookie, number_of_dcs, nodes_per_dc, connect_dc_or_not, rep1|rep2|rep3"
	exit
else
	AllSystemNodes=$1
    SystemNodesArray=($AllSystemNodes)
	Cookie=$2
	NumberDC=$3
	NodesPerDC=$4
    NodesToUse=$((NumberDC * NodesPerDC))
	AllNodes=${SystemNodesArray[@]:0:$NodesToUse}
    AllNodes=`echo ${AllNodes[@]}`
    ConnectDCs=$5
    echo "Using" $AllNodes ", will connect DCs:" $ConnectDCs
    if [ "$6" = "rep1" ]; then
	echo "Benchmark rep1"
        BenchmarkType=1
    elif [ "$6" = "rep2" ]; then
	echo "Benchmark rep2"
        BenchmarkType=2
    elif [ "$6" = "rep3" ]; then
	echo "Benchmark rep3"
        BenchmarkType=3
    else
        echo "Wrong benchmark type!"
        exit
    fi
fi
./script/stopNodes.sh "$AllSystemNodes" 
./script/deployMultiDCs.sh "$AllNodes" $Cookie $ConnectDCs $NodesPerDC $BenchmarkType

##Replace benchmark configuration to include nodes
if [ $BenchmarkType -eq 1 ]; then
    FileName="examples/antidote_pb_biased_rep1.config"
    ./script/changePBConfig.sh "$AllNodes" $Cookie $FileName
elif [ $BenchmarkType -eq 2 ]; then
    FileName="examples/antidote_pb_biased_rep2.config"
    ./script/changePBConfig.sh "$AllNodes" $Cookie $FileName
else
    FileName="examples/antidote_pb_biased_rep3.config"
    ./script/changePBConfig.sh "$AllNodes" $Cookie $FileName
fi

#LoadFile="loadfile.config"
#./script/createLoadFile.sh $FileName $LoadFile
echo "No loading phase..."
#sudo ./basho_bench "$LoadFile"
echo "Benchmarking phase..."
sudo ./basho_bench $FileName 
