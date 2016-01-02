#!/bin/bash

## Only update slave 

./script/runSpeculaBench.sh 5 0 100 true false 1 specula_tests
./script/runSpeculaBench.sh 5 0 100 true false 10 specula_tests
./script/runSpeculaBench.sh 5 0 100 true true 1 specula_tests
./script/runSpeculaBench.sh 5 0 100 true true 10 specula_tests
./script/runSpeculaBench.sh 5 0 100 false false 1 specula_tests
./script/runSpeculaBench.sh 5 0 100 false true 1 specula_tests

./script/runSpeculaBench.sh 5 0 0 true false 1 specula_tests
./script/runSpeculaBench.sh 5 0 0 true false 10 specula_tests
./script/runSpeculaBench.sh 5 0 0 true true 1 specula_tests
./script/runSpeculaBench.sh 5 0 0 true true 10 specula_tests
./script/runSpeculaBench.sh 5 0 0 false false 1 specula_tests
./script/runSpeculaBench.sh 5 0 0 false true 1 specula_tests

./script/runSpeculaBench.sh 5 100 0 true false 1 specula_tests
./script/runSpeculaBench.sh 5 100 0 true false 10 specula_tests
./script/runSpeculaBench.sh 5 100 0 true true 1 specula_tests
./script/runSpeculaBench.sh 5 100 0 true true 10 specula_tests
./script/runSpeculaBench.sh 5 100 0 false false 1 specula_tests
./script/runSpeculaBench.sh 5 100 0 false true 1 specula_tests

exit

# Only update master
./script/runSpeculaBench.sh 1 100 0 true false 1 specula_tests
./script/runSpeculaBench.sh 1 100 0 true false 5 specula_tests
./script/runSpeculaBench.sh 1 100 0 true false 10 specula_tests
./script/runSpeculaBench.sh 1 100 0 true true 1 specula_tests
./script/runSpeculaBench.sh 1 100 0 true true 5 specula_tests
./script/runSpeculaBench.sh 1 100 0 true true 10 specula_tests
./script/runSpeculaBench.sh 1 100 0 false false 1 specula_tests

#Only update other 
./script/runSpeculaBench.sh 1 0 0 true false 1 specula_tests
./script/runSpeculaBench.sh 1 0 0 true false 5 specula_tests
./script/runSpeculaBench.sh 1 0 0 true false 10 specula_tests
./script/runSpeculaBench.sh 1 0 0 true true 1 specula_tests
./script/runSpeculaBench.sh 1 0 0 true true 5 specula_tests
./script/runSpeculaBench.sh 1 0 0 true true 10 specula_tests
./script/runSpeculaBench.sh 1 0 0 false false 1 specula_tests

#Low locality 
./script/runSpeculaBench.sh 1 0 50 true false 1 specula_tests
./script/runSpeculaBench.sh 1 0 50 true false 5 specula_tests
./script/runSpeculaBench.sh 1 0 50 true false 10 specula_tests
./script/runSpeculaBench.sh 1 0 50 true true 1 specula_tests
./script/runSpeculaBench.sh 1 0 50 true true 5 specula_tests
./script/runSpeculaBench.sh 1 0 50 true true 10 specula_tests
./script/runSpeculaBench.sh 1 0 50 false false 1 specula_tests

# High locality 
./script/runSpeculaBench.sh 1 80 10 true false 1 specula_tests
./script/runSpeculaBench.sh 1 80 10 true false 5 specula_tests
./script/runSpeculaBench.sh 1 80 10 true false 10 specula_tests
./script/runSpeculaBench.sh 1 80 10 true true 1 specula_tests
./script/runSpeculaBench.sh 1 80 10 true true 5 specula_tests
./script/runSpeculaBench.sh 1 80 10 true true 10 specula_tests
./script/runSpeculaBench.sh 1 80 10 false false 1 specula_tests
