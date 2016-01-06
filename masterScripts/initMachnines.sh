#!/bin/bash


sudo ./script/preciseTime.sh
sudo ./script/parallel_command.sh "sudo apt-get -y install libwww-perl"

if [ $# -eq 1 ]; then
    Clean=$1
else
    Clean=3
fi
if [ $Clean == 1 ]
then
echo "Only cleaning antidote"
./script/makeRel.sh local_specula_read 
#./script/makeRel.sh improve_commit 
#./script/makeRel.sh integrate_repl 
elif [ $Clean == 2 ]
then
echo "Only cleaning basho_bench"
./script/parallel_command.sh "cd basho_bench && git stash && git pull && sudo make"
./script/command_to_all.sh "./basho_bench/masterScripts/config.sh" 
./script/command_to_all.sh "cd ./basho_bench/ && sudo chown -R ubuntu specula_tests"
else
echo "Cleaning both"
./script/makeRel.sh local_specula_read
./script/parallel_command.sh "cd basho_bench && git stash && git pull && sudo make"
./script/command_to_all.sh "./basho_bench/masterScripts/config.sh" 
./script/command_to_all.sh "cd ./basho_bench/ && sudo chown -R ubuntu specula_tests"
fi

./script/copy_to_all.sh ./script/allnodes ./basho_bench/script 
./script/command_to_all.sh "./basho_bench/masterScripts/config.sh" 
