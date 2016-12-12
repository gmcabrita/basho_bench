#!/usr/bin/env bash
# This script is meant to be run by the coordinating machine,
# to start the benchmarks in all the benchmark nodes, and collect their results.
# each node sends its results through scp, and they're put in a directory of the form basho_bench/tests/fmk-bench-date-time
# the basho bench script for individual nodes can be found in this directory/worker-runFMKbench.sh

# It assumes (IMPORTANT!!!)
# 1) Machines have a key in ~/.ssh/known hosts, so ssh does not prompt for passwords
# 2) there exists a file, bench-nodes-list.txt, in this directory with the list of IP addresses of the nodes that will run basho_bench
BenchNodes=`cat script/FMKe/bench-nodes-list.txt`

# Use the following line if one can obtain the public IP address of this machine from its adapter.
    if [ -z "$MY_IP" ]; then
    MY_IP=$(ifconfig en4 | grep inet | grep -v inet6 | awk '{print $2}')    # Otherwise, get the public IP
    #Worker_IP=$(dig +short myip.opendns.com @resolver1.opendns.com.)
fi


    # Otherwise, get the public IP
    #MY_IP=$(dig +short myip.opendns.com @resolver1.opendns.com.)
    # The IP address of the master node is sent to the worker nodes.
    # They use it to scp their results once they're done with their bench

if [ -z "$BenchDuration" ]; then
    BenchDuration=1
fi

# check that the script was called with the right parameters
if [ -z "$RUNFMKSETUP" ]; then
  echo "--##--Master ${MY_IP}: missing parameter: RUNFMKSETUP"
  echo "--##--Master ${MY_IP}: Run like: RUNFMKSETUP=<TRUE/FALSE> master-runBenchmarkStarter.sh"
  else
########################################################
    # Verify that all nodes can receiven ssh connections
##########################################################
#   First, check that master node is ssh-able
    RunCommand="ssh -q -o ConnectTimeout=2 -o StrictHostKeyChecking=no ${USER}@${MY_IP} exit"
    echo "--##--Master ${MY_IP}: checking master ssh connectivity with command:"
    echo "--##--Master ${MY_IP}: ${RunCommand}"
    eval $RunCommand
    if [ "$?" = 0  ]; then
        echo "--##--Master ${MY_IP}: SSH working fine on master!: ${MY_IP}"
    else
        echo "--##--Master ${MY_IP}: Unable to SSH master node ${MY_IP}, check what you're doing! good bye!"
        exit 1
    fi

#   Second, check that worker nodes are ssh-able

    for Item in ${BenchNodes}
    do
        RunCommand="ssh -q -o ConnectTimeout=2 -o StrictHostKeyChecking=no ${USER}@${Item} exit"
        echo "--##--Master ${MY_IP}: sending ssh command to ${Item} to verify connectivity as:"
        echo "--##--Master ${MY_IP}: ${RunCommand}"
        eval $RunCommand
        if [ "$?" = 0  ]; then
            echo "--##--Master ${MY_IP}: SSH working fine on : ${Item}"
        else
            echo "--##--Master ${MY_IP}: Unable to SSH node ${Item}, check what you're doing! good bye!"
            exit 1
        fi
    done
    echo "--##--Master ${MY_IP}: Great! All worker nodes can receive SSH connections."


#########################################################
# create a directory to store the test results...
# this directory is used by the worker nodes, to send their results via scp
# #########################################################

    DateTime=`date +%Y-%m-%d-%H-%M-%S`
    BenchResultsDirectory=~/basho_bench/tests/fmk-bench-${DateTime}
    mkdir -p $BenchResultsDirectory
    echo "--##--Master ${MY_IP}: Created dir to receive results: ${BenchResultsDirectory}"

    #####################################################
    # Send the command to start benchmarking to each node:
    #####################################################
for Item in ${BenchNodes}
    do
        RunCommand="ssh -o StrictHostKeyChecking=no ${USER}@${Item} Worker_IP=${Item} BenchResultsDirectory=${BenchResultsDirectory} MasterNodeIp=${MY_IP} RUNFMKSETUP=${RUNFMKSETUP} ~/basho_bench/script/FMKe/worker-runFMKbench.sh"
        echo "--##--Master ${MY_IP}: sending ssh command to ${Item} to run benchmark as:"
        echo "--##--Master ${MY_IP}: ${RunCommand}"
        eval $RunCommand &
    done


    #####################################################
    # Sleep until the benchmark ends:
    #####################################################
    Time=`date +%H-%M-%S`
    echo "--##--Master ${MY_IP}: Now its: ${Time} Gonna sleep ${BenchDuration} minutes..."
    sleep $((BenchDuration*60))
#    sleep a bit more if we setup fmke
    if [ "$RUNFMKSETUP" = TRUE ] ; then
        sleep 100
    fi

    echo "--##--Master ${MY_IP}:good nap! up now!"


    #####################################################
    # Now let's wait to collect the results from all workers
    #####################################################
    echo "--##--Master ${MY_IP}: cding into tests directory"
    cd ${BenchResultsDirectory}
    pwd
    Numfiles=$(eval "\ls -afq | wc -l")
    ReceivedFiles=$((Numfiles-2))

    echo "--##--Master ${MY_IP}: Waiting until all ${NumBenchNodes} worker nodes send their results..."
    until [  $ReceivedFiles = $NumBenchNodes ]; do
             sleep 2
             echo "--##--Master ${MY_IP}:Received result files so far...: ${ReceivedFiles}), missing "$((NumBenchNodes-ReceivedFiles))"}"
             Numfiles=$(eval "\ls -afq | wc -l")
             let ReceivedFiles=$((Numfiles-2))
         done
    echo "--##--Master ${MY_IP}: Done collecting results from all ${NumBenchNodes} nodes, gonna merge them into a single one...\n\n\n"

    #####################################################
    # Merge results in the test directory into a single one and create the results file image
    #####################################################
    # Call the merge results script
    CommandToRunMergeScript="BenchResultsDirectory=$BenchResultsDirectory ~/basho_bench/script/FMKe/master-mergeResults.sh"
    echo "--##--Master ${MY_IP}: Calling merge script with command:"
    echo "--##--Master ${MY_IP}: $CommandToRunMergeScript"
    eval $CommandToRunMergeScript

    # Create an image with the summary
    CommandToBuildPng="Rscript --vanilla priv/summary.r -i $BenchResultsDirectory/summary"
    echo "--##--Master ${MY_IP}: Processing results into a summary.png file..."
    echo "--##--Master ${MY_IP}: $CommandToBuildPng"
    cd ~/basho_bench/
    eval $CommandToBuildPng
    echo "--##--Master ${MY_IP}: DONE, see your results!!!"
    open $BenchResultsDirectory/summary/summary.png
fi
