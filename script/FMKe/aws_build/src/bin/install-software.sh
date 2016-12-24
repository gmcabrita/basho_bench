#!/bin/bash
# This script installs software necessary for later steps on an amazon virtual
# machine.

set -e # Any subsequent(*) commands which fail will cause the shell script
       # to exit immediately

sudo apt-get update
sudo apt-get --assume-yes upgrade
sudo apt-get --assume-yes install build-essential autoconf git libssl* openssh-server awscli r-base erlang

# needed to later build the PNG image
sudo chown ubuntu /usr/local/lib/R/site-library/
