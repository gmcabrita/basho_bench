#!/bin/bash

String=$1
IFS=',' read -ra Array <<< "$String"
Sum=0
for i in "${Array[@]}"; do
    Sum=$((Sum+i))
done
echo $Sum
