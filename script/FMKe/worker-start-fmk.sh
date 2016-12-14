#!/usr/bin/env bash
# Call this like IP=1.2.3.4 worker-start-antidote.sh

if [ -z "$CleanMake" ]; then
    CleanMake=TRUE
fi
if [ -z "$GitBranch" ]; then
    GitBranch="master"
fi

echo "----Worker $IP ----: cd ~/FMKe/"
cd ~/FMKe/
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
echo "----Worker $IP ----: IP=$IP ANTIDOTE_ADDRESS=$IP IP=$IP INSTANCE_NAME=fmk ./_build/default/rel/fmk/bin/env foreground"
ANTIDOTE_ADDRESS=$IP IP=$IP INSTANCE_NAME=fmk ./_build/default/rel/fmk/bin/env foreground