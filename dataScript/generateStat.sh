#!/bin/bash

Folder=$1
SubFolder=`ls -d $Folder/20*`
Output=summary
for F in $SubFolder
do
    Config=`cat $F/config`
    Committed=`cat $F/specula_out | awk -F ',' '{print $4}' | awk '{S+=$1}END{print S}'`
    Aborted=`cat $F/specula_out | awk -F ',' '{print $5}' | awk '{S+=$1}END{print S}'`
    ReadAborted=`cat $F/stat | awk -F ' ' '{print $2}'`
    SpeculaAborted=`cat $F/stat | awk -F ' ' '{print $4}'`
    CascadeAborted=`cat $F/stat | awk -F ' ' '{print $6}'`
    RealCommitted=$((Committed - ReadAborted - SpeculaAborted - CascadeAborted))
    echo "$Config" $RealCommitted $SpeculaAborted $ReadAborted $CascadeAborted $Aborted >> $Output
done
