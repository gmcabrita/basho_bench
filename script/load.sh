#!/bin/bash

cd script
sudo erl -pa script -name load@localhost -setcookie antidote -run load load_tpcc $1 $2 $3 -run init stop
cd -
