#!/usr/bin/env bash
# Call this like IP=1.2.3.4 worker-start-fmk.sh

chmod +x ~/basho_bench//script/FMKe/*
chmod +x ~/FMKe/bin/*
chmod +x ~/antidote/bin/*

if [ -z "$CLEANMAKE" ]; then
    CLEANMAKE=TRUE
fi
if [ -z "$GITBRANCH" ]; then
    GITBRANCH="master"
fi
if [ -z "$ANTIDOTE_ADDRESS" ]; then
    echo "Error: missing list of comma separated antidote IP addresses"
    exit 2
fi
if [ -z "$ANTIDOTE_PB_PORT" ]; then
    echo "Error: missing list of comma separated antidote PB ports"
    exit 2
fi


echo "----Worker $IP ----: cd ~/FMKe/"
cd ~/FMKe/
if [ CLEANMAKE=TRUE ]; then

    echo "----Worker $IP ----: git checkout $GITBRANCH"
    git checkout $GITBRANCH
    echo "----Worker $IP ----: git pull"
    git pull
    echo "----Worker $IP ----: make relclean"
    make relclean
    echo "----Worker $IP ----: make rel"
    make rel
fi
echo "----Worker $IP ----: IP=$IP ANTIDOTE_ADDRESS=$ANTIDOTE_ADDRESS IP=$IP INSTANCE_NAME=fmk ./_build/default/rel/fmk/bin/env foreground"
ANTIDOTE_ADDRESS=$ANTIDOTE_ADDRESS ANTIDOTE_PB_PORT=$ANTIDOTE_PB_PORT IP=$IP INSTANCE_NAME=fmk nohup ./_build/default/rel/fmk/bin/env foreground &
