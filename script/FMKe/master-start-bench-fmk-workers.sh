#!/usr/bin/env bash
# This script sends an ssh command to the nodes in an input file
# The nodes should have installed antidote under ~/antidote

# INPUT:
# 1) NodesListFile: THE filename with the list of nodes
# 2) The username of SSH to access the nodes
# 3) The private key to be used to connect to the nodes


# Example: if we want to make rel in antidote, we will call this script like: NodesListFile="~/basho_bench/script/FMKe/antidote-nodes-list.txt" ./master-start-antidote-workers.sh

if [ -z "$NodesListFile" ]; then
    NodesListFile=~/basho_bench/script/FMKe/antidote-nodes-list.txt
fi
if [ -z "$PrivateKey" ]; then
    PrivateKey=~/.ssh/antidote.pem
fi
chmod 600 $PrivateKey


SshOptions="-o StrictHostKeyChecking=no -i $PrivateKey"

Nodes=`cat ${NodesListFile}`
echo "Nodes are: ${Nodes}"


for Item in ${Nodes}
do
    Command="ssh $SshOptions $USER@$Item 'bash -s' < IP=${Item} ~/basho_bench/script/FMK/worker-start-antidote.sh"
    echo "Sending ssh command to ${Item}:"
    echo "${Command}"
    eval $Command &
done