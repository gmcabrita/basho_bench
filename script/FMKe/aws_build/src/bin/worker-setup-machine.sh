#!/bin/bash
# This script is going to be executed inside an amazon virtual machine and will
# clone and build a list of required git repositories.

set -e # Any subsequent(*) commands which fail will cause the shell script
       # to exit immediately

# add ESL as a repository
sudo wget -c -O- http://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc | sudo apt-key add -
sudo echo "deb http://packages.erlang-solutions.com/ubuntu xenial contrib" | sudo tee -a /etc/apt/sources.list.d/erlang_solutions.list > /dev/null

sudo apt-get update
sudo apt-get --assume-yes upgrade
sudo apt-get --assume-yes install build-essential autoconf git r-base erlang

# needed to later build the PNG image
sudo chown ubuntu /usr/local/lib/R/site-library/

 # env
 BIN_DIR=`pwd`
 cd $BIN_DIR

 #############################  antidote @############################
 ANTIDOTE_DIR=$HOME/antidote
if [ -z "$ANTIDOTE_BRANCH" ]; then
    ANTIDOTE_BRANCH="aws-ccrdts"
fi

if [ -d "$ANTIDOTE_DIR" ]; then
    echo "[SCRIPT] Antidote directory has been found in this node. Pulling latest changes..."
    # Control will enter here if $DIRECTORY exists.
    cd $ANTIDOTE_DIR

    git checkout $ANTIDOTE_BRANCH
    git pull
else
    echo "[SCRIPT] Antidote repository not found. Cloning repository..."
    git clone https://github.com/gmcabrita/antidote

    cd $ANTIDOTE_DIR
    git checkout $ANTIDOTE_BRANCH

fi
PUBLIC_NODE_IP=`curl checkip.amazonaws.com`
echo "{public_ip, {$PUBLIC_NODE_IP}}" > ./config/node-address.config
sed -ie 's/\./,/g' ./config/node-address.config
echo "." >> ./config/node-address.config

make rel
cd $BIN_DIR

 ########################## basho_bensh ##############################
 BASHO_BENSH_DIR=$HOME/basho_bench

 if [ -d "$BASHO_BENSH_DIR" ]; then
     echo "[SCRIPT] Basho Bench directory has been found in this node. Pulling latest changes..."

     cd $BASHO_BENSH_DIR
     git checkout antidote_pb_ccrdts_aws
     git pull
     make all
 else
     echo "[SCRIPT] Basho Bench repository not found. Cloning repository..."
     git clone https://github.com/gmcabrita/basho_bench
     cd $BASHO_BENSH_DIR
     git checkout antidote_pb_ccrdts_aws
     make all
 fi
