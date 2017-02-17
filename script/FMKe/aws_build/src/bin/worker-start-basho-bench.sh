#!/usr/bin/env bash
# Call this like IP=1.2.3.4 worker-start-fmk.sh

pkill epmd
pkill beam

BB_DIR="/home/ubuntu/basho_bench"

if [ -z "$CLEANMAKE" ]; then
    CLEANMAKE=TRUE
fi
if [ -z "$GITBRANCH" ]; then
    GITBRANCH="antidote_pb_ccrdts_aws"
fi
if [ -z "$CONFIG_FILE" ]; then
    CONFIG_FILE="antidote_ccrdts_topkd_only"
fi

if [ CLEANMAKE=TRUE ]; then
    cd $BB_DIR
    git checkout $GITBRANCH
    git pull
    make all
fi
cd -
/home/ubuntu/basho_bench/_build/default/bin/basho_bench /home/ubuntu/basho_bench/examples/${CONFIG_FILE}.config -n "${NODE_NAME}" &
