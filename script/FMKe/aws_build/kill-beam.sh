#!/usr/bin/env bash

PRIVATEKEY=$1
KEY_FILE_NAME=$(basename $PRIVATEKEY)
IP_ADDR_LIST=$(echo $* | cut -d' ' -f2-)
SSH_OPTIONS="-o StrictHostKeyChecking=no -i $PRIVATEKEY"
USER=ubuntu

USAGE="$0 <PATH TO PVT KEY> <LIST OF BASHO BENCH IPS>"

# check that the script was called with the right parameters
if [[ ! -e $1 ]]; then
    echo "Private key error: $1: no such file"
    exit 2
fi;


WORKER_SCRIPT="./src/bin/worker-kill-beam.sh"
REMOTE_WORKER_SCRIPT="/home/ubuntu/worker-kill-beam.sh"

echo "[SCRIPT]: Killing all beam processes in the remote machine..."
for IP_ADDR in $IP_ADDR_LIST; do
    echo "[SCRIPT]: Killing all beam processes in ${IP_ADDR}..."
    scp ${SSH_OPTIONS} ${WORKER_SCRIPT} ${USER}@${IP_ADDR}:${REMOTE_WORKER_SCRIPT};
    ssh ${SSH_OPTIONS} ${USER}@${IP_ADDR} chmod u+x ${REMOTE_WORKER_SCRIPT};
    ssh $SSH_OPTIONS $USER@${IP_ADDR} ${REMOTE_WORKER_SCRIPT} &
done

echo "[SCRIPT]: Done!"
