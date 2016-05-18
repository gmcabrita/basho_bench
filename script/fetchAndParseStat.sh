#!/bin/bash

AllNodes=`cat ./script/allnodes`
Folder=$1
./script/parallel_command.sh "cd basho_bench && sudo rm -f ./stat && sudo ./script/parseStat.sh localhost ."
exit
./script/copyFromAll.sh stat ./basho_bench/ $Folder                 

Header="ReadAborted,ReadInvalid,CertAborted,CascadeAborted,Committed,Whatever,SpeculaRead,Whatever,NOCommitLP,NOCommitRP,NOAbortLP,NOAbortRP,PCommitLP,PCommitRP,PAbortLP,PAbortRP,GCommitLP,GCommitRP,GAbortLP,GAbortRP"
echo "$Header" >> $Folder/stat

for N in $AllNodes
do
cat $Folder/$N-stat >> $Folder/stat
rm $Folder/$N-stat
done

