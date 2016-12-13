#!/usr/bin/env bash
# Call this like IP=1.2.3.4 worker-start-antidote.sh
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
echo "----Worker $IP ----: git pull"
git pull
echo "----Worker $IP ----: make relclean"
make relclean
echo "----Worker $IP ----: make rel"
make rel
echo "----Worker $IP ----: INSTANCE_NAME=antidote ~/antidote/_build/default/rel/antidote/bin/env stop"
INSTANCE_NAME=antidote ~/antidote/_build/default/rel/antidote/bin/env stop
echo "----Worker $IP ----: IP=$IP INSTANCE_NAME=antidote ~/antidote/_build/default/rel/antidote/bin/env foreground"
IP=$IP INSTANCE_NAME=antidote ~/antidote/_build/default/rel/antidote/bin/env foreground




