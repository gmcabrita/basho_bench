#!/usr/bin/env bash
# Call this like IP=1.2.3.4 worker-start-antidote.sh

ANTIDOTE_DIR="/home/ubuntu/antidote"

if [ -z "$CLEANMAKE" ]; then
    CLEANMAKE=TRUE
fi
if [ -z "$GITBRANCH" ]; then
    GITBRANCH="aws-ccrdts"
fi

if [ CLEANMAKE=TRUE ]; then
    cd $ANTIDOTE_DIR
    echo "[SCRIPT] KILLING ALL ERLANG PROCESSES ON REMOTE MACHINES..."
    pkill epmd
    pkill beam
    rm -rf _build/default/lib/antidote*
    rm -rf _build/default/rel
    rm -f rebar.lock
    ./rebar3 update
    ./rebar3 upgrade

    echo "----Worker $IP ----: git checkout $GITBRANCH"
    git checkout $GITBRANCH
    echo "----Worker $IP ----: git pull"
    git pull
    git reset --hard origin/$GITBRANCH
    echo "[SCRIPT] DELETING DATA FROM PREVIOUS BENCHMARKS, IF ANY..."
    echo "----Worker $IP ----: make relclean"
    rm -rf _build/default/rel
    make relclean
    echo "[SCRIPT] REGENERATING RELX RELEASE..."
    echo "----Worker $IP ----: make rel"
    make rel
    cd -
fi

echo "----Worker $IP ----: IP=$IP INSTANCE_NAME=antidote ~/antidote/_build/default/rel/antidote/bin/env foreground"
IP=$IP INSTANCE_NAME=antidote nohup ~/antidote/_build/default/rel/antidote/bin/env foreground &
