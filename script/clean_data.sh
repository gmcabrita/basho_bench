#!/bin/bash

cd script
FirstNode=` head -1 allnodes`
sudo erl -pa script -name setter@localhost -setcookie antidote -run clean_data clean_data $FirstNode -run init stop
cd - 
