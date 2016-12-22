#!/bin/bash
set -e # Any subsequent(*) commands which fail will cause the shell script
       # to exit immediately

# env
BIN_DIR=`pwd`

# apt antidote s ubuntu dependencies
sudo apt-get update
sudo apt-get --assume-yes upgrade
sudo apt-get --assume-yes install build-essential
sudo apt-get --assume-yes install autoconf
sudo apt-get --assume-yes install git
sudo apt-get --assume-yes install libssl*
sudo apt-get --assume-yes install ncurses*
sudo apt-get --assume-yes install default-jre
sudo apt-get --assume-yes install default-jdk
sudo apt-get --assume-yes install x-window-system
sudo apt-get --assume-yes install flex
sudo apt-get --assume-yes install openssh-server
sudo apt-get --assume-yes install libexpat1
sudo apt-get --assume-yes install libexpat1-dev
sudo apt-get --assume-yes install awscli
sudo apt-get --assume-yes install r-base
sudo apt-get --assume-yes install erlang

cd $BIN_DIR

#############################  antidote @############################
ANTIDOTE_DIR=$HOME

cd $ANTIDOTE_DIR
git clone https://github.com/SyncFree/antidote

cd antidote
git checkout build-local-cluster
make rel

cd $BIN_DIR

############################## FMKe #################################
FMKE_DIR=$HOME

cd $FMKE_DIR
git clone https://github.com/goncalotomas/FMKe
cd FMKe
git checkout perf-and-errors
make rel

cd $BIN_DIR

########################## basho_bensh ##############################
BASHO_BENSH_DIR=$HOME

cd $BASHO_BENSH_DIR
git clone https://github.com/SyncFree/basho_bench
cd basho_bench
git checkout antidote_pb_fmk
make all
