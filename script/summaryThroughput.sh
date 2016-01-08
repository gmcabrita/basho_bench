#!/bin/bash
Folder=$1
rm $Folder/specula_out
touch $Folder/tmp
Files=`ls $Folder/[0-9]* | grep -v prep`
for F in $Files
do
paste $1/tmp $F | awk -F ',' '{print $1",",$2",",($3+$7)",",($4+$8)",",($5+$9)" "}' > $1/tmp1
mv $1/tmp1 $1/tmp
done
mv $1/tmp $1/specula_out
