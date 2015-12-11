#!/bin/bash

cd ./localScripts
sudo erl -pa script -name setter@localhost -setcookie antidote -run getStat get_stat $1 -run init stop 
cd - 
