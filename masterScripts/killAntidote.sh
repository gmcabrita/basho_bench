#!/bin/bash


sudo ./script/parallel_command.sh "`cat ./script/exceptme`" "sudo pkill -f antidote"
sudo ./script/parallel_command.sh "`cat ./script/exceptme`" "sudo pkill -f basho"
