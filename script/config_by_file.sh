#!/bin/bash
set -e

while read -r Line
do
    echo "Line is" $Line
    List=($Line)
    if [ ${List[0]} == "tpcc" ]
    then
	File="examples/tpcc.config"
    elif [ ${List[0]} == "load" ]
    then
	File="examples/load.config"
    else
	File="../antidote/rel/antidote/antidote.config"
    fi
    Key=${List[1]}
    Value=${List[2]}
    sudo sed -i -e 's/{'"$Key"'.*/{'"$Key"', '"$Value"'}./' "$File"
done < ./config 

