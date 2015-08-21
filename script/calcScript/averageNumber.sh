#!/bin/bash

Folders=`./script/calcScript/getFolders.sh $1 $2`
Sum=0
FoldArray=(${Folders})
for File in "${FoldArray[@]}"
do
    File=$File"/summary.csv"
    Sum=$((Sum+`./script/calcScript/sumColumn.sh $File 4`))
done
echo "Sum is $Sum"
