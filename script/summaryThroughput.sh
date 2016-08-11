#!/bin/bash
Folder=$1
touch $Folder/tmp
Files=`ls $Folder/summary.csv-*`
for F in $Files
do
paste $1/tmp $F | awk -F ',' '{print $1",",$2",",($3+$8)",",($4+$9)",",($5+$10)",",($6+$11)}' > $1/tmp1
mv $1/tmp1 $1/tmp
done
tail -n+2 $1/tmp >>  $1/specula_out
