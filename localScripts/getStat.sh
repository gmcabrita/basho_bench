#!/bin/bash

cd ./localScripts
sudo erl -pa script -name stat@localhost -setcookie antidote -noshell -run getStat get_stat $1 -run init stop 
cd - 
