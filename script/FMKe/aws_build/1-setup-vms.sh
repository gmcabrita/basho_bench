#!/bin/bash
# author goncalotomas
# This script prepares several amazon virtual machines for later use.
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

# env
PRIVATEKEY=$1
KEY_FILE_NAME=$(basename $PRIVATEKEY)

IP_ADDR_LIST=$(echo $* | cut -d' ' -f2-)

INSTALL_SOFTWARE_SCRIPT="./src/bin/install-software.sh"
BUILD_REPOSITORIES_SCRIPT="./src/bin/clone-repositories.sh"
REMOTE_INSTALL_SOFTWARE_SCRIPT="~/install-software.sh"
REMOTE_BUILD_REPOSITORIES_SCRIPT="~/clone-repositories.sh"

SSH_USERNAME=ubuntu
SSH_OPTIONS="-i $PRIVATEKEY -o StrictHostKeyChecking=no"

echo "[SCRIPT] RUNNING SCRIPT TO SETUP MULTIPLE REMOTE MACHINES..."

# copy scripts to remote machines and add execute permission
echo "[SCRIPT] 1/3: COPYING REQUIRED SCRIPTS TO REMOTE MACHINES..."
for IP_ADDR in $IP_ADDR_LIST; do
    scp $SSH_OPTIONS $INSTALL_SOFTWARE_SCRIPT $SSH_USERNAME@$IP_ADDR:$REMOTE_INSTALL_SOFTWARE_SCRIPT
    scp $SSH_OPTIONS $BUILD_REPOSITORIES_SCRIPT $SSH_USERNAME@$IP_ADDR:$REMOTE_BUILD_REPOSITORIES_SCRIPT
    ssh $SSH_OPTIONS $SSH_USERNAME@$IP_ADDR chmod u+x $REMOTE_INSTALL_SOFTWARE_SCRIPT
    ssh $SSH_OPTIONS $SSH_USERNAME@$IP_ADDR chmod u+x $REMOTE_BUILD_REPOSITORIES_SCRIPT
done

# install required software on remote machines
echo "[SCRIPT] 2/3: INSTALLING NECESSARY SOFTWARE ON REMOTE MACHINES..."
for IP_ADDR in $IP_ADDR_LIST; do
    ssh $SSH_OPTIONS $SSH_USERNAME@$IP_ADDR nohup $REMOTE_INSTALL_SOFTWARE_SCRIPT
done

echo "[SCRIPT] 3/3: CLONING ALL REQUIRED REPOSITORIES TO REMOTE MACHINES..."
for IP_ADDR in $IP_ADDR_LIST; do
    ssh $SSH_OPTIONS $SSH_USERNAME@$IP_ADDR nohup $REMOTE_BUILD_REPOSITORIES_SCRIPT &
done

echo "[SCRIPT] Done. All remote machines have the required software stack and repositories."
