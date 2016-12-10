#!/usr/bin/env bash
# This script is meant to be run by the coordinating machine,
# to start the benchmarks in all the benchmark nodes, and collect their results.
# It assumes there exists a file, basho_bench-nodes-list.txt, in this directory with the list of IP addresses of the nodes that will run basho_bench

BenchNodes=`cat script/FMKe/basho_bench-nodes-list.txt`

for Item in ${BenchNodes}
do
    RunCommand="ssh alek@${Item} ~/basho_bench/script/FMKe/worker-runFMKbench.sh"
    echo "sending ssh command to ${Item} to run benchmark as:"
    echo "${RunCommand}"
    eval $RunCommand
    echo "done benchmarking, getting the results..."
    SCPCommand="scp alek@${Item}:~/basho_bench/*.tar ."
    echo $SCPCommand
    eval $SCPCommand
done



#echo ssh -t -o StrictHostKeyChecking=no -i key root@$Item /root/basho_bench"$I"/basho_bench/script/runSimpleBenchmark.sh $BenchmarkType $I $BenchmarkFile ${ReadsNumber[$ReadWrite]} ${WritesNumber[$ReadWrite]} $NumberDC $NodesPerDC $DCNum
