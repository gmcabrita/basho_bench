#!/bin/bash
set -e

while read -r Line
do
    echo "Line is" $Line
    List=($Line)
    Type=0
    if [ ${List[0]} == "tpcc" ]
    then
	File="examples/tpcc.config"
    elif [ ${List[0]} == "load" ]
    then
	File="examples/load.config"
    elif [ ${List[0]} == "app_config" ]
    then
        File="../antidote/rel/antidote/etc/app.config"
	Type=1
    else
	File="../antidote/rel/antidote/antidote.config"
    fi
    Key=${List[1]}
    Value=${List[2]}
    if [ "$Type" -eq 0 ]
    then
        sudo sed -i -e 's/{'"$Key"'.*/{'"$Key"', '"$Value"'}./' "$File"
    else
        sudo sed -i -e 's/{'"$Key"'.*/{'"$Key"', '"$Value"'},/' "$File"
    fi
done < ./config 

