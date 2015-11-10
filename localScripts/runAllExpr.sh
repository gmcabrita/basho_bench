#!/bin/bash
set -e
Folder="speculaResults/"
KeyRanges="500 1000 2000 5000"

sudo ./localScripts/changeForAllDevs.sh do_specula true
sudo ./localScripts/changeForAllDevs.sh do_repl false 
for KeyRange in $KeyRanges
do
    sudo ./localScripts/changeConfig.sh ./examples/antidote_pb.config key_range $KeyRange
    sudo ./localScripts/runAnExpr.sh
done

sudo ./localScripts/changeForAllDevs.sh do_repl true 
for KeyRange in $KeyRanges
do
    sudo ./localScripts/changeConfig.sh ./examples/antidote_pb.config key_range $KeyRange
    sudo ./localScripts/runAnExpr.sh
done

sudo ./localScripts/changeForAllDevs.sh do_specula false
sudo ./localScripts/changeForAllDevs.sh do_repl false 
for KeyRange in $KeyRanges
do
    sudo ./localScripts/changeConfig.sh ./examples/antidote_pb.config key_range $KeyRange
    sudo ./localScripts/runAnExpr.sh
done

sudo ./localScripts/changeForAllDevs.sh do_repl true 
for KeyRange in $KeyRanges
do
    sudo ./localScripts/changeConfig.sh ./examples/antidote_pb.config key_range $KeyRange
    sudo ./localScripts/runAnExpr.sh
done
