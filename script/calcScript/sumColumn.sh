#!/bin/bash

Column=`./script/calcScript/extractColumn.sh $1 $2`
Array=(${Column})
IFS=+ read <<< "${Array[*]}" 
((sum=REPLY))
echo $sum
