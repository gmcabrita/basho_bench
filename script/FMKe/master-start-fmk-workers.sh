#!/usr/bin/env bash
# This script sends an ssh command to the nodes in an input file
# The nodes should have installed fmk under ~/FMKe

# INPUT:
# 1) NodesListFile: THE filename with the list of nodes
# 2) The username of SSH to access the nodes
# 3) The private key to be used to connect to the nodes
# 4) CleanMake = TRUE/FALSE: make clean && make rel or not
# 5) GitBranch: the branch of git we want to checkout



if [ -z "$NodesListFile" ]; then
    NodesListFile=~/basho_bench/script/FMKe/bench-nodes-list.txt
fi
if [ -z "$CleanMake" ]; then
    CleanMake=TRUE
fi
if [ -z "$GitBranch" ]; then
    GitBranch="master"
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
    Command="ssh $SshOptions $USER@$Item GitBranch=${GitBranch} CleanMake=${CleanMake}IP=${Item} ~/basho_bench/script/FMKe/worker-start-fmk.sh"
    echo "Sending ssh command to ${Item}:"
    echo "${Command}"
    eval $Command &
done