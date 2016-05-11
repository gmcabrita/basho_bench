#!/bin/bash

cd script
FirstNode=` head -1 allnodes`
#Ip=`GET http://169.254.169.254/2014-11-05/meta-data/public-ipv4`
sudo erl -pa script -name setter@127.0.0.1 -setcookie antidote -run clean_data clean_data $FirstNode -run init stop
cd - 
