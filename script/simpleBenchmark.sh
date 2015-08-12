#!/bin/bash


AllNodes=`head -$1 script/allnodes`

##Replace benchmark configuration to include nodes
FileName="examples/antidote_pb.config"
./script/changePBConfig.sh "$AllNodes" $FileName $2 

#LoadFile="loadfile.config"
#./script/createLoadFile.sh $FileName $LoadFile
echo "No loading phase..."
#sudo ./basho_bench "$LoadFile"
echo "Benchmarking phase..."
sudo ./basho_bench $FileName
