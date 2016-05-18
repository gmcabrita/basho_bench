#!/bin/bash



cd ./tests/current/
for l in `ls *latency`; do cat $l >> bench_latency;  done
cd -

sudo ./scripts/getCDF.sh

cd ../antidote/rel/antidote
rm ant_latency
for l in `ls *latency`; do cat $l >> ant_latency;  done
