#!/usr/bin/env bash
# Call this like IP=1.2.3.4 worker-start-antidote.sh

chmod +x ~/basho_bench//script/FMKe/*
chmod +x ~/FMKe/bin/*
chmod +x ~/antidote/bin/*

if [ -z "$CLEANMAKE" ]; then
    CLEANMAKE=TRUE
fi
if [ -z "$GITBRANCH" ]; then
    GITBRANCH="build-local-cluster"
fi

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

echo "----Worker $IP ----: IP=$IP INSTANCE_NAME=antidote ~/antidote/_build/default/rel/antidote/bin/env foreground"
IP=$IP INSTANCE_NAME=antidote nohup ~/antidote/_build/default/rel/antidote/bin/env foreground &
