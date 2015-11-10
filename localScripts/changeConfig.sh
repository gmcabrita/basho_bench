#!/bin/bash

File=$1
sudo sed -i '' -e "s/{$2.*/{$2, $3}./" "$File"
#sudo sed -i '' -e "s/{key_range.*/key_range, 100/" "$File"
