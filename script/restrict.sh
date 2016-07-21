#!/bin/bash

pids=`ps -eLo nice,tid,args | grep [b]eam | awk -F ' ' '{print $2}'`
if [ $1 == 1 ];
then
    echo pintoone 
    for pid in $pids; do taskset -pc 1 $pid; done
else
    echo here
    for pid in $pids; do taskset -pc 0,1 $pid; done
fi
