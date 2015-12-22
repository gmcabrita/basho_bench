#!/bin/bash

Folder=`ls ./tests/ | tail -2 | head -1`
rm ./tests/prep
cp ./tests/$Folder/prep ./tests/prep
