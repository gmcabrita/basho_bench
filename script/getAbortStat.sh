#!/bin/bash

AllNodes=$1
Folder=$2
cd script
Results=`sudo erl -pa script -name abort_stat@localhost -setcookie antidote -run getAbortStat get_stat $1 -run init stop`
cd - 
echo $Results >> $Folder/abort_stat

