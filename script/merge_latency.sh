#!/bin/bash


Ip=`GET http://169.254.169.254/2014-11-05/meta-data/public-ipv4`

cd ./tests/current/
for l in `ls *latency`; do cat $l >> latency_bench;  done
cd -

sudo ./script/getCDF.sh $Ip

cd ../antidote/rel/antidote
rm latency_ant
for l in `ls *latency`; do cat $l >> latency_ant;  done
rm ./*latency

