#!/usr/bin/env bash

# THIS SCRIPT SHOULD BE CALLED LIKE:
###### NumDCs=1 ~/basho_bench/script/FMKe/master-antidote_multidc_deploy


# This benchmark will create an antidote setup of a number of DCs and machines per DC.

# It assumes:
# There is a list of all nodes running antidote in antidote-nodes-list.txt (or another file that can be passed as an argument)
# Antidote is running on all those ips with the following erlang node name: 'antidote@IP'

# INPUT:
# 1) NumDCs: The number of Datacenters to be deployed
#    The script will divide evenly the number of machines on the list in the number of DCs]
#   If this is not provided, one Cluster of all antidote machines will be created



# This is only necessary when running on OS X, erlang 19
# might be removed, but won't harm otherwise...
PATH="$PATH:/opt/local/lib/erlang/erts-8.1/bin/"
chmod +x ~/basho_bench//script/FMKe/*
chmod +x ~/FMKe/bin/*
chmod +x ~/antidote/bin/*
SshOptions="-o StrictHostKeyChecking=no -i $PrivateKey"


if [ -z "$NumDCs" ]; then
    echo "---MASTER-CONNECT-DCS: NumDCs was not provided, defaulting to a single DC"
    NumDCs=1
fi

if [ -z "$NodesListFile" ]; then
    NodesListFile=~/basho_bench/script/FMKe/antidote-nodes-list.txt
fi

########################################################
    # Divide the list of nodes into clusters
##########################################################
NodesList=`cat $NodesListFile`
NumNodes=${#NodesList[@]}

if [ $NumNodes < 2 ]; then
    echo "---MASTER-CONNECT-DCS: Too little nodes to work with: $NodesList"
    exit 255
fi

echo "---MASTER-CONNECT-DCS: Nodes are: ${NodesList}"

NodesInThisCluster=""
ClusterHeads=""

i=1
for Item in ${NodesList}
do
#    Make a list of nodes that will conform each cluster
    NodesInThisCluster="'antidote@"$Item"' "$NodesInThisCluster""
    if [ $((NumNodes % i)) = 0 ]; then
#        Make a cluster with this nodes
        echo "---MASTER-CONNECT-DCS: Will make a cluster with this nodes: $NodesInThisCluster"
        Command="cd ~/antidote"
        echo "---MASTER-CONNECT-DCS: $Command"
        eval $Command
        Command="~/antidote/bin/join_cluster_script.erl $NodesInThisCluster"
        echo "---MASTER-CONNECT-DCS: $Command"
        eval $Command
        NodesInThisCluster=""
        i=$((i + 1))
   else
        if [ $((NumNodes % i)) = 1 ]; then
#        This is the first node of a cluster, will use it as cluster head, for later running the connect_dcs script
        ClusterHeads="'antidote@"$Item"' "$ClusterHeads""
    fi
done