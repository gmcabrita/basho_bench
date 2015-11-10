#!/bin/bash

cd ./localScripts
sudo erl -pa script -name setter@localhost -setcookie antidote -run getStat get_stat -run init stop 
cd - 
