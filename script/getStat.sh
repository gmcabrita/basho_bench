#!/bin/bash


FileName="stat/"`date +"%Y-%m-%d-%H:%M:%S"`
sudo erl -pa script -name setter@localhost -setcookie antidote -run getStat get_vnode_stat -run init stop > $FileName 
sudo erl -pa script -name setter@localhost -setcookie antidote -run getStat get_time_stat -run init stop >> $FileName 
