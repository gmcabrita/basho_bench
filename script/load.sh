#!/bin/bash

cd script
#Ip=`GET http://169.254.169.254/2014-11-05/meta-data/public-ipv4`
sudo erl -pa script -name load@127.0.0.1 -setcookie antidote -noshell -run load load_tpcc $1 $2 $3 -run init stop
cd -
