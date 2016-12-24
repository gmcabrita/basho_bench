#!/bin/bash
# author goncalotomas
# This script joins N already running replicas of antidote into a cluster (DC).
# You should pass in a list of (public) IP addresses as arguments to the script,
# as well as the following environment variables:
# PRIVATEKEY: used to ssh into the amazon virtual machines. Every machine is
# assumed to be accessible using one key.

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

SSH_OPTIONS="-i $PRIVATEKEY -o StrictHostKeyChecking=no"

IP_ADDR_LIST=$(echo $* | cut -d' ' -f2-)
REQUESTER=IP_ADDR_LIST[0]

for IP_ADDR in $IP_ADDR_LIST; do
    ACCUM="$ACCUM antidote@$IP_ADDR"
done

echo "[SCRIPT] RUNNING SCRIPT TO JOIN MULTIPLE ANTIDOTE REPLICAS IN A CLUSTER..."

SSH_USERNAME=ubuntu
SSH_OPTIONS="-i $PRIVATEKEY -o StrictHostKeyChecking=no"

echo "[SCRIPT] RUNNING THE JOIN CLUSTER SCRIPT FROM $REQUESTER..."

Command="ssh $SSH_OPTIONS $USER@$Item ~/antidote/bin/join_cluster_script.erl $ACCUM"
echo "Requesting antidote cluster join on node ${REQUESTER}, using the following command:"
echo "${Command}"
eval $Command &

sleep 15 # cluster creation takes a while
echo "[SCRIPT] Done. The specified antidote replicas are now joined in a cluster."
