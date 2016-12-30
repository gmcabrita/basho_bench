#!/usr/bin/env bash
# author goncalotomas

PRIVATEKEY=$1
KEY_FILE_NAME=$(basename $PRIVATEKEY)
IP_ADDR_LIST=$(echo $* | cut -d' ' -f2-)
SSH_OPTIONS="-o StrictHostKeyChecking=no -i $PRIVATEKEY"
USER=ubuntu

if [ -z "$BENCHDURATION" ]; then
    BENCHDURATION=1
fi
if [ -z "$POPULATE_ANTIDOTE" ]; then
    POPULATE_ANTIDOTE=FALSE
fi

USAGE="FMK_HTTP_ADDRESSES=<LIST OF CSV IP ADDRESSES> FMK_HTTP_PORTS=<LIST OF CSV PORTS> NUM_CLIENTS=<NUM THREADS PER MACHINE> [POPULATE_ANTIDOTE=<TRUE|FALSE>] [BENCHDURATION=<MINUTES>] $0 <PATH TO PVT KEY> <LIST OF BASHO BENCH IPS>"

# check that the script was called with the right parameters
if [[ ! -e $1 ]]; then
    echo "Private key error: $1: no such file"
    exit 2
fi;
if [ -z "$FMK_HTTP_ADDRESSES" ]; then
  echo "Missing list of FMKe server addresses"
  echo ${USAGE}
  exit 1
fi
if [ -z "$FMK_HTTP_PORTS" ]; then
  echo "Missing list of FMKe server ports"
  echo ${USAGE}
  exit 1
fi
if [ -z "$NUM_CLIENTS" ]; then
  echo "Missing number of basho bench clients"
  echo ${USAGE}
  exit 1
fi

##########################################################
    # Verify that all nodes are reachable
##########################################################
echo "[SCRIPT]: STEP 1/5: TESTING REQUIREMENTS FOR EVERY BASHO BENCH NODE..."

for IP_ADDR in $IP_ADDR_LIST; do
    ssh ${SSH_OPTIONS} ${USER}@${IP_ADDR} exit
    if [ "$?" = 0  ]; then
        echo "[SCRIPT] SSH connection OK for ${IP_ADDR}"
    else
        echo "[SCRIPT] SSH connection NOT OK for ${IP_ADDR}, aborting..."
        exit 1
    fi
done

echo "[SCRIPT] All nodes are reachable."

REMOTE_FILE_PATH="/home/ubuntu/basho_bench/_build/default/bin/basho_bench"

#########################################################
# CONNECTION TEST STAGE                                 #
#########################################################
for IP_ADDR in $IP_ADDR_LIST; do
    ssh ${SSH_OPTIONS} ${USER}@${IP_ADDR} cat $REMOTE_FILE_PATH > /dev/null 2>&1
    if [ "$?" = 0  ]; then
        echo "[SCRIPT] Basho bench escript is present for ${IP_ADDR}"
    else
        echo "[SCRIPT] Basho bench is not compiled in node ${IP_ADDR}, aborting..."
        exit 1
    fi
done

echo "[SCRIPT]: STEP 1/5: Done. All nodes contain a compiled version of basho_bench."

FMK_ADDRESS_ARR=(`echo ${FMK_HTTP_ADDRESSES}`);
FMK_PORT_ARR=(`echo ${FMK_HTTP_PORTS}`);
IP_ARR=(`echo ${IP_ADDR_LIST}`);

## TODO

#########################################################
# BENCHMARK CONFIGURATION STAGE                         #
#########################################################
echo "[SCRIPT]: STEP 2/5: Editing configuration files for each bench node..."
REMOTE_CONFIG_FILE="/home/ubuntu/basho_bench/examples/fmkclient.config"
WORKER_SCRIPT="./src/bin/worker-configure-benchmark.sh"
REMOTE_WORKER_SCRIPT="/home/ubuntu/worker-configure-benchmark.sh"

for IP_ADDR in $IP_ADDR_LIST; do
    echo "[SCRIPT] KILLING ALL ERLANG PROCESSES ON REMOTE MACHINES..."
    ssh ${SSH_OPTIONS} ${USER}@${IP_ADDR} pkill beam
    echo "[SCRIPT]: Copying configuration script to remote machine..."
    scp ${SSH_OPTIONS} ${WORKER_SCRIPT} ${USER}@${IP_ADDR}:${REMOTE_WORKER_SCRIPT}
    ssh ${SSH_OPTIONS} ${USER}@${IP_ADDR} chmod u+x ${REMOTE_WORKER_SCRIPT}
    echo "[SCRIPT]: Configuration script copied successfully."
    echo "[SCRIPT]: Running configuration script..."
    ssh ${SSH_OPTIONS} ${USER}@${IP_ADDR} NUM_CLIENTS=${NUM_CLIENTS} BENCHDURATION=${BENCHDURATION} IP_ADDR=${IP_ADDR} FMK_HTTP_ADDRESSES=${FMK_HTTP_ADDRESSES} FMK_HTTP_PORTS=${FMK_HTTP_PORTS} REMOTE_CONFIG_FILE=${REMOTE_CONFIG_FILE} ${REMOTE_WORKER_SCRIPT}
    echo "[SCRIPT]: Node ${IP_ADDR} has been successfully configured."
done

#########################################################
# ANTIDOTE POPULATION STAGE                             #
#########################################################
echo "[SCRIPT]: STEP 3/5: Checking if antidote population has been requested..."
if [ "$POPULATE_ANTIDOTE" = TRUE ]
    then
        echo "[SCRIPT] Antidote population has been requested."
        for IP_ADDR in $IP_ADDR_LIST; do
            REQUESTER=${IP_ADDR} ## This is dumb but it works, so I'll leave it
        done;
        for IP_ADDR in $FMK_HTTP_ADDRESSES; do
            POPULATION_ADDRESS=${IP_ADDR} ## This is dumb but it works, so I'll leave it
        done;

        echo "[SCRIPT] Running antidote population worker script..."
        LOCAL_POPULATION_SCRIPT="./src/bin/fmk_setup_script.erl"
        REMOTE_POPULATION_SCRIPT="/home/ubuntu/fmk_setup_script.erl"
        POPULATOR_NODE_REF="populate@${REQUESTER}"
        FMK_NODE_REF="fmk@${POPULATION_ADDRESS}"
        scp $SSH_OPTIONS $LOCAL_POPULATION_SCRIPT $USER@$IP_ADDR:$REMOTE_POPULATION_SCRIPT
        ssh $SSH_OPTIONS $USER@$IP_ADDR chmod u+x $REMOTE_POPULATION_SCRIPT
        ssh $SSH_OPTIONS $USER@$IP_ADDR $REMOTE_POPULATION_SCRIPT $POPULATOR_NODE_REF $FMK_NODE_REF
    else
        echo "[SCRIPT] No request for antidote population found. Continuing..."
fi
echo "STEP 3/5: Done."

#########################################################
# BENCHMARKING STAGE                                    #
#########################################################
echo "[SCRIPT]: STEP 4/5: Starting benchmarks..."
for IP_ADDR in $IP_ADDR_LIST; do
    REMOTE_BENCHMARK_COMMAND="/home/ubuntu/basho_bench/_build/default/bin/basho_bench ${REMOTE_CONFIG_FILE}"
    echo "[SCRIPT]: Starting benchmark in node ${IP_ADDR}..."
    ssh ${SSH_OPTIONS} ${USER}@${IP_ADDR} $REMOTE_BENCHMARK_COMMAND &
done
echo "[SCRIPT]: BENCHMARKS STARTED IN ALL NODES."

echo "[SCRIPT]: I'm just gonna sleep for ${BENCHDURATION} minutes, ok? BRB."

sleep $((${BENCHDURATION}*60))

echo "ZzzzzZZZzzzzzZZZzzzzz...."

sleep 30

echo "I'm back and I feel so alive! Let's get those results!!"

echo "[SCRIPT]: STEP 5/5: Fetching and merging results..."
# mkdir benchmarks
# cd benchmarks
# for IP_ADDR in $IP_ADDR_LIST; do
#     REMOTE_BENCHMARK_COMMAND="/home/ubuntu/basho_bench/_build/default/bin/basho_bench ${REMOTE_CONFIG_FILE}"
#     echo "[SCRIPT]: Starting benchmark in node ${IP_ADDR}..."
#     ssh ${SSH_OPTIONS} ${USER}@${IP_ADDR} $REMOTE_BENCHMARK_COMMAND &
# done
# for IP_ADDR in $IP_ADDR_LIST; do
#     REMOTE_BENCHMARK_COMMAND="/home/ubuntu/basho_bench/_build/default/bin/basho_bench ${REMOTE_CONFIG_FILE}"
#     echo "[SCRIPT]: Starting benchmark in node ${IP_ADDR}..."
#     ssh ${SSH_OPTIONS} ${USER}@${IP_ADDR} $REMOTE_BENCHMARK_COMMAND &
# done
#     #####################################################
#     # Now let's wait to collect the results from all workers
#     #####################################################
#     echo "--##--Master ${MY_IP}: cding into tests directory"
#     cd ${BenchResultsDirectory}
#     pwd
#     Numfiles=$(eval "\ls -afq | wc -l")
#     InitNumFiles=$Numfiles
#     ReceivedFiles=$((Numfiles-InitNumFiles))
#
#     echo "--##--Master ${MY_IP}: Waiting until all ${NumBenchNodes} worker nodes send their results..."
#     until [  $ReceivedFiles = $NumBenchNodes ]; do
#              sleep 2
#              echo "--##--Master ${MY_IP}:Received result files so far...: ${ReceivedFiles}), missing "$((NumBenchNodes-ReceivedFiles))"}"
#              Numfiles=$(eval "\ls -afq | wc -l")
#              let ReceivedFiles=$((Numfiles-InitNumFiles))
#          done
#     echo "--##--Master ${MY_IP}: Done collecting results from all ${NumBenchNodes} nodes, gonna merge them into a single one...\n\n\n"
#
#     #####################################################
#     # Merge results in the test directory into a single one and create the results file image
#     #####################################################
#     # Call the merge results script
#     CommandToRunMergeScript="BenchResultsDirectory=$BenchResultsDirectory ~/basho_bench/script/FMKe/master-mergeResults.sh"
#     echo "--##--Master ${MY_IP}: Calling merge script with command:"
#     echo "--##--Master ${MY_IP}: $CommandToRunMergeScript"
#     eval $CommandToRunMergeScript
#
#     # Create an image with the summary
#     CommandToBuildPng="Rscript --vanilla priv/summary.r -i $BenchResultsDirectory/summary"
#     echo "--##--Master ${MY_IP}: Processing results into a summary.png file..."
#     echo "--##--Master ${MY_IP}: $CommandToBuildPng"
#     cd ~/basho_bench/
#     eval $CommandToBuildPng
#     echo "--##--Master ${MY_IP}: DONE, see your results!!!"
#     open $BenchResultsDirectory/summary/summary.png
# fi
