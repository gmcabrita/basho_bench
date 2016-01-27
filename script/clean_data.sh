#!/bin/bash

cd script
sudo erl -pa script -name setter@localhost -setcookie antidote -run clean_data clean_data $1 -run init stop
cd - 
