#!/bin/bash

List1=$1
Div1=$2
Div2=$3
Result=`./script/calcScript/sumList.sh $1`
Result1=$((Result / Div1 ))
Result2=$((Result1 / Div2 ))
echo $Result2
