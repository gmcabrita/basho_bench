#!/bin/bash
# author goncalotomas
# This script starts N replicas of FMKe on previously configured remote vms.
# You should pass in a list of (public) IP addresses as arguments to the script,
# as well as the following environment variables:
# PRIVATEKEY: used to ssh into the amazon virtual machines. Every machine is
# assumed to be accessible using one key.
# GITBRANCH: FMKe branch to start up
# ANTIDOTE_ADDRESS: list of CSV antidote IP addresses
# ANTIDOTE_PB_PORT: list of CSV antidote PB ports (must be 1-to-1 with previous list)

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
if [ -z "$ANTIDOTE_ADDRESS" ]; then
    echo "Error: missing list of comma separated antidote IP addresses"
    exit 2
fi
if [ -z "$ANTIDOTE_PB_PORT" ]; then
    echo "Error: missing list of comma separated antidote PB ports"
    exit 2
fi

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

echo "[SCRIPT] RUNNING SCRIPT TO START MULTIPLE ANTIDOTE REPLICAS..."

SSH_USERNAME=ubuntu
SSH_OPTIONS="-i $PRIVATEKEY -o StrictHostKeyChecking=no"

for IP_ADDR in $IP_ADDR_LIST; do
    Command="ssh $SSH_OPTIONS $USER@$Item GITBRANCH=${GITBRANCH} CLEANMAKE=${CLEANMAKE} IP=${IP_ADDR} ANTIDOTE_ADDRESS=${ANTIDOTE_ADDRESS} ANTIDOTE_PB_PORT=${ANTIDOTE_PB_PORT} ~/basho_bench/script/FMKe/aws_build/src/bin/worker-start-fmk.sh"
    echo "[SCRIPT] Starting antidote on node ${IP_ADDR}, using the following command:"
    echo "[SCRIPT] ${Command}"
    eval $Command &
done

sleep 10 # antidote takes a while to boot up.
echo "[SCRIPT] Done. FMKe has been launched on the specified replicas."
