#!/usr/bin/env bash
# author goncalotomas

PRIVATEKEY=$1
KEY_FILE_NAME=$(basename $PRIVATEKEY)
IP_ADDR_LIST=$(echo $* | cut -d' ' -f2-)
SSH_OPTIONS="-o StrictHostKeyChecking=no -i $PRIVATEKEY"
USER=ubuntu

if [ -z "$BENCHDURATION" ]; then
    BENCHDURATION=5
fi

if [ -z "$CONFIG_FILE" ]; then
    CONFIG_FILE="antidote_ccrdts_topkd_only"
fi

if [ -z "$NUM_KEYS" ]; then
    NUM_KEYS=1000
fi

USAGE="CONFIG_FILE=<BASHO BENCH CONFIG FILE> ANTIDOTE_NODES=<LIST OF ANTIDOTE NODES> NUM_CLIENTS=<NUM THREADS PER MACHINE> [NUM_KEYS=<NUM KEYS>] [BENCHDURATION=<MINUTES>] $0 <PATH TO PVT KEY> <LIST OF BASHO BENCH IPS>"

# check that the script was called with the right parameters
if [[ ! -e $1 ]]; then
    echo "Private key error: $1: no such file"
    exit 2
fi;
if [ -z "$ANTIDOTE_NODES" ]; then
  echo "Missing list of Antidote nodes"
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
echo "[SCRIPT]: STEP 1/4: TESTING REQUIREMENTS FOR EVERY BASHO BENCH NODE..."

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

echo "[SCRIPT]: STEP 1/4: Done. All nodes contain a compiled version of basho_bench."

#########################################################
# BENCHMARK CONFIGURATION STAGE                         #
#########################################################
ANTIDOTE_NODES_ARR=(`echo ${ANTIDOTE_NODES}`);
ANTIDOTE_NODES_ARR_SIZE=${#ANTIDOTE_NODES_ARR[@]}
IP_ARR=(`echo ${IP_ADDR_LIST}`);

echo "[SCRIPT]: STEP 2/4: Editing configuration files for each bench node..."
REMOTE_CONFIG_FILE="/home/ubuntu/basho_bench/examples/${CONFIG_FILE}.config"
WORKER_SCRIPT="./src/bin/worker-configure-benchmark.sh"
REMOTE_WORKER_SCRIPT="/home/ubuntu/worker-configure-benchmark.sh"
WORKER_BENCH_SCRIPT="./src/bin/worker-start-basho-bench.sh"
REMOTE_WORKER_BENCH_SCRIPT="/home/ubuntu/worker-start-basho-bench.sh"

for index in "${!IP_ARR[@]}"; do
    ## ASSIGN EACH BASHO BENCH TO EACH FMK IN A ROUND ROBIN FASHION
    ## SO THAT WE ALLOW THE CASE WHERE #BENCHENODES > #FMKNODES
    echo "[SCRIPT]: Copying configuration script to remote machine..."
    scp ${SSH_OPTIONS} ${WORKER_SCRIPT} ${USER}@${IP_ARR[$index]}:${REMOTE_WORKER_SCRIPT}
    ssh ${SSH_OPTIONS} ${USER}@${IP_ARR[$index]} chmod u+x ${REMOTE_WORKER_SCRIPT}
    echo "[SCRIPT]: Configuration script copied successfully."
    echo "[SCRIPT]: Copying runnable worker script to remote machine..."
    scp ${SSH_OPTIONS} ${WORKER_BENCH_SCRIPT} ${USER}@${IP_ARR[$index]}:${REMOTE_WORKER_BENCH_SCRIPT}
    ssh ${SSH_OPTIONS} ${USER}@${IP_ARR[$index]} chmod u+x ${REMOTE_WORKER_BENCH_SCRIPT}
    echo "[SCRIPT]: Runnable worker script copied successfully."
    echo "[SCRIPT]: Running configuration script..."
    ssh ${SSH_OPTIONS} ${USER}@${IP_ARR[$index]} NUM_CLIENTS=${NUM_CLIENTS} BENCHDURATION=${BENCHDURATION} NUM_KEYS=${NUM_KEYS} IP_ADDR=${IP_ARR[$index]} ANTIDOTE_NODES=${ANTIDOTE_NODES_ARR[$(($index % $ANTIDOTE_NODES_ARR_SIZE))]} REMOTE_CONFIG_FILE=${REMOTE_CONFIG_FILE} ${REMOTE_WORKER_SCRIPT}
done

#########################################################
# BENCHMARKING STAGE                                    #
#########################################################
REMOTE_BB_SCRIPT=${REMOTE_WORKER_BENCH_SCRIPT}
echo "[SCRIPT]: STEP 3/3: Starting benchmarks..."
for IP_ADDR in $IP_ADDR_LIST; do
    echo "[SCRIPT]: Starting benchmark in node ${IP_ADDR}..."
    ssh $SSH_OPTIONS $USER@${IP_ADDR} CONFIG_FILE=${CONFIG_FILE} GITBRANCH=${GITBRANCH} CLEANMAKE=${CLEANMAKE} NODE_NAME="bb@${IP_ADDR}" ${REMOTE_BB_SCRIPT} &
done
echo "[SCRIPT]: BENCHMARKS STARTED IN ALL NODES."

echo "[SCRIPT]: I'm just gonna sleep for ${BENCHDURATION} minutes, ok? BRB."

sleep $((${BENCHDURATION}*60))

echo "ZzzzzZZZzzzzzZZZzzzzz...."

sleep 45

echo "[SCRIPT]: Done!"
