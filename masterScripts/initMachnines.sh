#!/bin/bash



if [ $# -eq 1 ]; then
    Clean=$1
else
    Clean=4
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
elif [ $Clean == 3 ]
then
echo "Only initing"
sudo ./script/parallel_command.sh 'echo 127.0.0.1 `hostname` | sudo tee --append /etc/hosts'
sudo ./script/preciseTime.sh
sudo ./script/parallel_command.sh "sudo apt-get update && sudo apt-get -y install libwww-perl"
sudo ./script/parallel_command.sh "cd basho_bench && git config --global user.email 'mars.leezm@gmail.com'"
sudo ./script/parallel_command.sh "cd basho_bench && git config --global user.name 'marsleezm'"
elif [ $Clean == 4 ]
then
echo "Just config!!"
else
echo "Cleaning all"
sudo ./script/parallel_command.sh 'echo 127.0.0.1 `hostname` | sudo tee --append /etc/hosts'
sudo ./script/preciseTime.sh
sudo ./script/parallel_command.sh "sudo apt-get update && sudo apt-get -y install libwww-perl"
sudo ./script/parallel_command.sh "sudo apt-get -y install sshpass"
sudo ./script/parallel_command.sh "cd basho_bench && git config --global user.email 'mars.leezm@gmail.com'"
sudo ./script/parallel_command.sh "cd basho_bench && git config --global user.name 'marsleezm'"
./script/makeRel.sh local_specula_read
./script/parallel_command.sh "cd basho_bench && git stash && git pull && sudo make"
./script/command_to_all.sh "./basho_bench/masterScripts/config.sh" 
./script/command_to_all.sh "cd ./basho_bench/ && sudo chown -R ubuntu specula_tests"
fi

sudo ./script/copy_to_all.sh ./script/allnodes ./basho_bench/script 
sudo ./script/command_to_all.sh "./basho_bench/masterScripts/config.sh" 
