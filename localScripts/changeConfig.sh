#!/bin/bash

File=$1
echo "Configuring $2 to" "$3" "for" $File
if [ $# == 3 ]
then
  sudo sed -i -e 's/{'"$2"'.*/{'"$2"', '"$3"'}./' "$File"
elif [ $4 == 1 ]
then
sudo sed -i -e 's/{'"$2"'.*/{'"$2"', '"$3"'},/' "$File"
elif [ $4 == 2 ]
then
sudo sed -i -e 's/{'"$2"'.*/{'"$2"', '"$3"'}./' "$File"
else
sudo sed -i -e 's/{'"$2"'.*/{'"$2"', '"$3"'}/' "$File"
fi
#sudo sed -i '' -e "s/{key_range.*/key_range, 100/" "$File"
