#!/bin/bash
# author goncalotomas
# This script starts N replicas of antidote on previously configured remote vms.
# You should pass in a list of (public) IP addresses as arguments to the script,
# as well as the following environment variables:
# PRIVATEKEY: used to ssh into the amazon virtual machines. Every machine is
# assumed to be accessible using one key.
# GITBRANCH: antidote branch to start up
# CLEANMAKE = TRUE/FALSE: make relclean && make rel or not

set -e # Any subsequent(*) commands which fail will cause the shell script
       # to exit immediately

# args checking
if [[ $# -lt 2 ]]; then
    echo "Error: usage $0 <private_key_file> <IP_ADDR_LIST> ..."
    exit 2
fi;

if [[ ! -e $1 ]]; then
    echo "Error: $1: no such file"
    exit 2
fi;
if [ -z "$CLEANMAKE" ]; then
    CLEANMAKE=TRUE
fi
if [ -z "$GITBRANCH" ]; then
    GITBRANCH="build-local-cluster"
fi

# env
PRIVATEKEY=$1
KEY_FILE_NAME=$(basename $PRIVATEKEY)

IP_ADDR_LIST=$(echo $* | cut -d' ' -f2-)

SSH_OPTIONS="-i $PRIVATEKEY -o StrictHostKeyChecking=no"
SSH_USERNAME=ubuntu

echo "[SCRIPT] RUNNING SCRIPT TO START MULTIPLE ANTIDOTE REPLICAS..."

echo "[SCRIPT] KILLING ALL ERLANG PROCESSES ON REMOTE MACHINES..."
for IP_ADDR in $IP_ADDR_LIST; do
    Command="ssh $SSH_OPTIONS $USER@$IP_ADDR pkill beam"
    eval $Command
done

echo "[SCRIPT] DELETING DATA FROM PREVIOUS BENCHMARKS, IF ANY..."
for IP_ADDR in $IP_ADDR_LIST; do
    Command="ssh $SSH_OPTIONS $USER@$IP_ADDR make -C ~/antidote relclean"
    eval $Command &
done

echo "[SCRIPT] REGENERATING RELX RELEASE..."
for IP_ADDR in $IP_ADDR_LIST; do
    Command="ssh $SSH_OPTIONS $USER@$IP_ADDR make -C ~/antidote rel"
    eval $Command &
done

for IP_ADDR in $IP_ADDR_LIST; do
    Command="ssh $SSH_OPTIONS $USER@$Item GITBRANCH=${GITBRANCH} CLEANMAKE=${CLEANMAKE} IP=${IP_ADDR} ~/basho_bench/script/FMKe/aws_build/src/bin/worker-start-antidote.sh"
    echo "[SCRIPT] Starting antidote on node ${IP_ADDR}, using the following command:"
    echo "[SCRIPT] ${Command}"
    eval $Command &
done

sleep 10 # antidote takes a while to boot up.
echo "[SCRIPT] Done. Antidote has been launched on the specified replicas."
