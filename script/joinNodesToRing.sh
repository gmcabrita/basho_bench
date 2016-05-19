#!/bin/bash


if [ $# -eq 1 ]
then
    First=`head -1 ./script/allnodes`
    Others=`awk -v var="$1" 'NR>=2&&NR<=var' ./script/allnodes`
    echo Parameter:$1 , joinig $Others to $First
else
    First=$1
    Others=$2
    echo "Joinig "$Others " to "$First
fi

Join="sudo antidote/rel/antidote/bin/antidote-admin cluster join antidote@$First"
PlanAndCommit="sudo antidote/rel/antidote/bin/antidote-admin cluster plan && sudo antidote/rel/antidote/bin/antidote-admin cluster commit"
for Node in $Others
do
./script/command_to_all.sh "$Node" "$Join"
sleep 2.5 
done
./script/command_to_all.sh "$First" "$PlanAndCommit"

