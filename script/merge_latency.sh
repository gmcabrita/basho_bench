#!/bin/bash


Ip=`GET http://169.254.169.254/2014-11-05/meta-data/public-ipv4`

#cd ./tests/current/
#for l in `ls *latency`; do cat $l >> latency_bench;  done
#cd -

cd ../antidote/rel/antidote
rm ./*latency*
cd -
sudo ./script/getCDF.sh $Ip

for i in `seq 1 10`;
do
if ls ../antidote/rel/antidote/*final-latency 1> /dev/null 2>&1; then
    echo "Files do exist"
    break
else
    echo "Files do not exist, try again!!"
    sudo ./script/getCDF.sh $Ip
fi
done

cd ../antidote/rel/antidote
for l in `ls *final-latency`; do echo $l >> latency_final; cat $l >> latency_final;  done
for l in `ls *percv-latency`; do echo $l >> latency_percv; cat $l >> latency_percv;  done
