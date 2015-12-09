#!/bin/bash
set -e
File="diffPrepare/diff"
echo "First expr with no specula"
./localScripts/diffPrepare.sh 1 $File false 800 
./localScripts/diffPrepare.sh 1 $File false 800 
./localScripts/diffPrepare.sh 1 $File false 800 
echo "First expr with specula"
./localScripts/diffPrepare.sh 1 $File true 800 
./localScripts/diffPrepare.sh 1 $File true 800 
./localScripts/diffPrepare.sh 1 $File true 800 

echo "Snd expr with no specula"
./localScripts/diffPrepare.sh 10 $File false 400 
./localScripts/diffPrepare.sh 10 $File false 400 
./localScripts/diffPrepare.sh 10 $File false 400 
echo "Snd expr with specula"
./localScripts/diffPrepare.sh 10 $File true 400 
./localScripts/diffPrepare.sh 10 $File true 400 
./localScripts/diffPrepare.sh 10 $File true 400 

./localScripts/diffPrepare.sh 10 $File false 800 
./localScripts/diffPrepare.sh 10 $File false 800 
./localScripts/diffPrepare.sh 10 $File false 800 
./localScripts/diffPrepare.sh 10 $File true 800 
./localScripts/diffPrepare.sh 10 $File true 800 
./localScripts/diffPrepare.sh 10 $File true 800 

./localScripts/diffPrepare.sh 10 $File false 8000 
./localScripts/diffPrepare.sh 10 $File false 8000 
./localScripts/diffPrepare.sh 10 $File true 8000
./localScripts/diffPrepare.sh 10 $File true 8000
