#!/bin/bash
set -e

while read -r Line
do
    #echo "Line is" $Line
    List=($Line)
    Type=0
    if [ ${List[0]} == "tpcc" ]
    then
	File="examples/tpcc.config"
    File2=$File
    elif [ ${List[0]} == "rubis" ]
    then
	File="examples/rubis.config"
    File2=$File
    elif [ ${List[0]} == "load" ]
    then
	File="examples/load.config"
    File2=$File
    elif [ ${List[0]} == "micro" ]
    then
        File="examples/micro.config"
        File2=$File
    elif [ ${List[0]} == "app_config" ]
    then
        File="../antidote/rel/antidote/etc/app.config"
        File2=$File
	    Type=1
    else
	    File="../antidote/rel/antidote/antidote.config"
        File2="../antidote/rel/files/antidote.config"
    fi
    Key=${List[1]}
    Value=${List[2]}
    if [ "$Type" -eq 0 ]
    then
	if [ $Key == "all_nodes" ]
	then
	    A=`cat ./script/allnodes`
	    S=""
	    for L in $A; do S=$S','\'$L\'; done; 
	    V=`echo "${S:1}"`
            sudo sed -i -e 's/{'"all_nodes"'.*/{'"all_nodes"', '["$V"]'}./' "$File"
            sudo sed -i -e 's/{'"all_nodes"'.*/{'"all_nodes"', '["$V"]'}./' "$File2"
	else
            sudo sed -i -e 's/{'"$Key"'.*/{'"$Key"', '"$Value"'}./' "$File"
            sudo sed -i -e 's/{'"$Key"'.*/{'"$Key"', '"$Value"'}./' "$File2"
	fi
    else
        sudo sed -i -e 's/{'"$Key"'.*/{'"$Key"', '"$Value"'},/' "$File"
        sudo sed -i -e 's/{'"$Key"'.*/{'"$Key"', '"$Value"'},/' "$File2"
    fi
done < ./config 

