#!/bin/bash

awk -v "line=$2" '{print $line}' $1  | sed 's/,//g' | sed -e '1d;14d'
