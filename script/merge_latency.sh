#!/bin/bash


Ip=`GET http://169.254.169.254/2014-11-05/meta-data/public-ipv4`

#cd ./tests/current/
#for l in `ls *latency`; do cat $l >> latency_bench;  done
#cd -

sudo ./script/getCDF.sh $Ip

cd ../antidote/rel/antidote
rm latency_final
rm latency_percv
for l in `ls *final-latency`; do echo $l >> latency_final; cat $l >> latency_final;  done
for l in `ls *percv-latency`; do echo $l >> latency_percv; cat $l >> latency_percv;  done
rm ./*latency

