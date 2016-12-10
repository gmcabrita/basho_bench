#!/usr/bin/env bash
# This script is meant to be run by the coordinating machine,
# to start the benchmarks in all the benchmark nodes, and collect their results.
# each node sends its results through scp, and they're put in a directory of the form basho_bench/tests/fmk-bench-date-time
# the basho bench script for individual nodes can be found in this directory/worker-runFMKbench.sh

# It assumes (IMPORTANT!!!)
# 1) Machines have a key in ~/.ssh/known hosts, so ssh does not prompt for passwords
# 2) there exists a file, basho_bench-nodes-list.txt, in this directory with the list of IP addresses of the nodes that will run basho_bench
BenchNodes=`cat script/FMKe/basho_bench-nodes-list.txt`

# The IP address of the master node is sent to the worker nodes.
# They use it to scp their results once they're done with their bench
# IMPORTANT: check that the obtained ip is ssh-able

# Use the following line if one can obtain the public IP address of this machine from its adapter.
MY_IP=$(ifconfig en0 | grep inet | grep -v inet6 | awk '{print $2}')
# Otherwise, get the public IP
#MY_IP=$(dig +short myip.opendns.com @resolver1.opendns.com.)

# create a directory to store the test results...
DateTime=`date +%Y-%m-%d-%H-%M-%S`
BenchResultsDirectory=~/basho_bench/tests/fmk-bench-${DateTime}
mkdir -p $BenchResultsDirectory
echo "Created dir to receive results: ${BenchResultsDirectory}"
# Send the command to start benchmarking to each node:
for Item in ${BenchNodes}
do
    RunCommand="ssh alek@${Item} ~/basho_bench/script/FMKe/worker-runFMKbench.sh ${MY_IP} ${BenchResultsDirectory}"
    echo "sending ssh command to ${Item} to run benchmark as:"
    echo "${RunCommand}"
    eval $RunCommand &
done



#echo ssh -t -o StrictHostKeyChecking=no -i key root@$Item /root/basho_bench"$I"/basho_bench/script/runSimpleBenchmark.sh $BenchmarkType $I $BenchmarkFile ${ReadsNumber[$ReadWrite]} ${WritesNumber[$ReadWrite]} $NumberDC $NodesPerDC $DCNum
