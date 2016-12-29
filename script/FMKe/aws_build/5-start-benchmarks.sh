#!/usr/bin/env bash
# author goncalotomas

if [ -z "$PrivateKey" ]; then
    PrivateKey=~/.ssh/antidotedbKeyPair.pem
fi

SshOptions="-o StrictHostKeyChecking=no -i $PrivateKey"
USER=ubuntu

## TODO READ NUMBER OF BASHO BENCH CLIENTS FROM ENV VARIABLE OR ARGUMENT
## TODO READ FMK SERVER ADDRESSES FROM ENV VARIABLE OR ARGUMENT
## TODO READ FMK SERVER PORTS FROM ENV VARIABLE OR ARGUMENT

if [ -z "$BENCHDURATION" ]; then
    BENCHDURATION=1
fi

# check that the script was called with the right parameters
if [ -z "$RUNFMKSETUP" ]; then
  echo "--##--Master ${MY_IP}: missing parameter: RUNFMKSETUP"14
  echo "--##--Master ${MY_IP}: Run like: MY_IP=<my_ip_ssh_enabled> RUNFMKSETUP=<TRUE/FALSE> master-runBenchmarkStarter.sh"
  else
########################################################
    # Verify that all nodes can receive ssh connections
##########################################################
echo "[SCRIPT]: STEP 1/5: TESTING REQUIREMENTS FOR EVERY BASHO BENCH NODE..."

for IP_ADDR in $IP_ADDR_LIST; do
    ssh ${SshOptions} ${USER}@${IP_ADDR} exit
    if [ "$?" = 0  ]; then
        echo "[SCRIPT] SSH connection OK for ${IP_ADDR}"
    else
        echo "[SCRIPT] SSH connection NOT OK for ${IP_ADDR}, aborting..."
        exit 1
    fi
done

echo "[SCRIPT] All nodes are reachable."

REMOTE_FILE_PATH="/home/ubuntu/basho_bench/_build/default/bin/basho_bench"

for IP_ADDR in $IP_ADDR_LIST; do
    ssh ${SshOptions} ${USER}@${IP_ADDR} cat $REMOTE_FILE_PATH
    if [ "$?" = 0  ]; then
        echo "[SCRIPT] Basho bench escript is present for ${IP_ADDR}"
    else
        echo "[SCRIPT] Basho bench is not compiled in node ${IP_ADDR}, aborting..."
        exit 1
    fi
done

echo "[SCRIPT]: STEP 1/5: Done. All nodes contain a compiled version of basho_bench."
echo "[SCRIPT]: STEP 2/5: Editing configuration files for each bench node..."

REMOTE_CONFIG_FILE="/home/ubuntu/basho_bench/test/fmkclient.config"

for IP_ADDR in $IP_ADDR_LIST; do
    echo "[SCRIPT]: Configuring FMK server addresses..."
    ssh ${SshOptions} ${USER}@${IP_ADDR} sed -ie 's#{fmk_server_addresses, \[\(\"\([0-9]\{1,3\}\.\)\{3\}\([0-9]\{1,3\}\)\{1\}\"\)\(,\(\"\([0-9]\{1,3\}\.\)\{3\}\([0-9]\{1,3\}\)\{1\}\"\)\)*\]}.#{fmk_server_addresses, ['"$DURATION"']}.#g' ${REMOTE_CONFIG_FILE} ${REMOTE_CONFIG_FILE} ## TODO MODIFY
    if [ "$?" = 0  ]; then
        echo "[SCRIPT] Successfully configured FMK server addresses."
    else
        echo "[SCRIPT] Could not write FMK server addresses to node ${IP_ADDR}, aborting..."
        exit 1
    fi
    echo "[SCRIPT]: Configuring FMK server ports..."
    ssh ${SshOptions} ${USER}@${IP_ADDR} sed -ie 's#{fmk_server_ports, \[[0-9]\+[,[0-9]\+]*\]}.#{fmk_server_ports, ['"$DURATION"']}.#g' ${REMOTE_CONFIG_FILE} ## TODO MODIFY
    if [ "$?" = 0  ]; then
        echo "[SCRIPT] Successfully configured FMK server ports."
    else
        echo "[SCRIPT] Could not write FMK server ports to node ${IP_ADDR}, aborting..."
        exit 1
    fi
    echo "[SCRIPT]: Setting number of clients (${NUM_CLIENTS})..."
    ssh ${SshOptions} ${USER}@${IP_ADDR} sed -ie 's#{concurrent, [0-9]\+}.#{concurrent, '"${NUM_CLIENTS}"'}.#g' ${REMOTE_CONFIG_FILE}
    if [ "$?" = 0  ]; then
        echo "[SCRIPT] Successfully configured number of basho bench clients."
    else
        echo "[SCRIPT] Basho bench is not compiled in node ${IP_ADDR}, aborting..."
        exit 1
    fi
    echo "[SCRIPT]: Setting benchmark duration (${BENCHDURATION})..."
    ssh ${SshOptions} ${USER}@${IP_ADDR} sed -ie 's#{duration, [0-9]\+}.#{duration, '"${BENCHDURATION}"'}.#g' ${REMOTE_CONFIG_FILE}
    if [ "$?" = 0  ]; then
        echo "[SCRIPT] Successfully configured number of basho bench clients."
    else
        echo "[SCRIPT] Basho bench is not compiled in node ${IP_ADDR}, aborting..."
        exit 1
    fi
    echo "[SCRIPT] Successfully completed configuration of node ${IP_ADDR}."
done

echo "[SCRIPT]: STEP 3/5: Checking if antidote population has been requested..."
if [ "$RUNFMKSETUP" = TRUE ]
    then
        echo "[SCRIPT] Antidote population has been requested. Populating antidote..."
        ~/basho_bench/script/FMKe/master-run_fmk_setup.sh
    else
        echo "[SCRIPT] No request for antidote population found. Continuing..."
fi
echo "STEP 3/5: Done."

echo "[SCRIPT]: STEP 4/5: Starting benchmarks..."

echo "[SCRIPT]: STEP 5/5: Fetching and merging results..."


#########################################################
# create a directory to store the test results...
# this directory is used by the worker nodes, to send their results via scp
# #########################################################

    DateTime=`date +%Y-%m-%d-%H-%M-%S`
    BenchResultsDirectory=~/basho_bench/tests/fmk-bench-${DateTime}
    mkdir -p $BenchResultsDirectory
    echo "--##--Master ${MY_IP}: Created dir to receive results: ${BenchResultsDirectory}"


    #####################################################
    # RUN THE FMK SETUP
    # IMPORTANT: THE FOLLOWING SCRIPTS ASSUMES THAT THE MASTER RUNS ON A MACHINE WHERE FMK IS RUNNING
    #####################################################
if [ "$RUNFMKSETUP" = TRUE ] ; then
        echo "--##--Master ${MY_IP}: Running FMK setup..."
        ~/basho_bench/script/FMKe/master-run_fmk_setup.sh
    fi
    #####################################################
    # Send the command to start benchmarking to each node:
    #####################################################
for Item in ${BenchNodes}
    do
        RunCommand="ssh ${SshOptions} ${USER}@${Item} Worker_IP=${Item} BenchResultsDirectory=${BenchResultsDirectory} MasterNodeIp=${MY_IP} ~/basho_bench/script/FMKe/worker-runFMKbench.sh"
        echo "--##--Master ${MY_IP}: sending ssh command to ${Item} to run benchmark as:"
        echo "--##--Master ${MY_IP}: ${RunCommand}"
        eval $RunCommand &
    done


    #####################################################
    # Sleep until the benchmark ends:
    #####################################################
    Time=`date +%H-%M-%S`
    echo "--##--Master ${MY_IP}: Now its: ${Time} Gonna sleep ${BENCHDURATION} minutes..."
    sleep $((BENCHDURATION*60))
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
    InitNumFiles=$Numfiles
    ReceivedFiles=$((Numfiles-InitNumFiles))

    echo "--##--Master ${MY_IP}: Waiting until all ${NumBenchNodes} worker nodes send their results..."
    until [  $ReceivedFiles = $NumBenchNodes ]; do
             sleep 2
             echo "--##--Master ${MY_IP}:Received result files so far...: ${ReceivedFiles}), missing "$((NumBenchNodes-ReceivedFiles))"}"
             Numfiles=$(eval "\ls -afq | wc -l")
             let ReceivedFiles=$((Numfiles-InitNumFiles))
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
