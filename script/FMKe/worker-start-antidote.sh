#!/usr/bin/env bash
# Call this like IP=1.2.3.4 worker-start-antidote.sh

if [ -z "$CleanMake" ]; then
    CleanMake=TRUE
fi
if [ -z "$GitBranch" ]; then
    GitBranch="build-local-cluster"
fi

echo "----Worker $IP ----: pkill beam"
pkill beam
echo "----Worker $IP ----: cd ~"
cd ~
echo "----Worker $IP ----: . ./r18b03/activate"
. ./r18b03/activate
echo "----Worker $IP ----: cd ~/basho_bench/"
cd ~/basho_bench/
echo "----Worker $IP ----: git pull"
git pull
echo "----Worker $IP ----: cd ~/antidote"
cd ~/antidote
if [ CleanMake=TRUE ]; then
    echo "----Worker $IP ----: git checkout $GitBranch"
    git checkout $GitBranch
    echo "----Worker $IP ----: git pull"
    git pull
    echo "----Worker $IP ----: make relclean"
    make relclean
    echo "----Worker $IP ----: make rel"
    make rel
fi
echo "----Worker $IP ----: IP=$IP INSTANCE_NAME=antidote ~/antidote/_build/default/rel/antidote/bin/env foreground"
IP=$IP INSTANCE_NAME=antidote ~/antidote/_build/default/rel/antidote/bin/env foreground




