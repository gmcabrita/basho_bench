#!/usr/bin/env bash
# Call this like IP=1.2.3.4 worker-start-antidote.sh
cd ~/basho_bench/
git pull
cd ~/antidote
git pull
make relclean
make rel
IP=${IP} INSTANCE_NAME=antidote ~/antidote/_build/default/rel/antidote/bin/env foreground




