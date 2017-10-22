#!/bin/bash

set -e

sudo apt-get update
sudo apt-get --assume-yes upgrade
sudo apt-get --assume-yes install build-essential autoconf git r-base erlang r-cran-ggplot2 r-cran-proto r-cran-plyr r-cran-getopt


# needed to later build the PNG image
sudo chown ubuntu /usr/local/lib/R/site-library/

 # env
 BIN_DIR=`pwd`
 cd $BIN_DIR

 #############################  antidote @############################
 ANTIDOTE_DIR=$HOME/antidote
if [ -z "$ANTIDOTE_BRANCH" ]; then
    ANTIDOTE_BRANCH="nupaxos"
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
     git checkout nupaxos
     git stash
     git pull
     make all
 else
     echo "[SCRIPT] Basho Bench repository not found. Cloning repository..."
     git clone https://github.com/gmcabrita/basho_bench
     cd $BASHO_BENSH_DIR
     git checkout nupaxos
     make all
 fi
