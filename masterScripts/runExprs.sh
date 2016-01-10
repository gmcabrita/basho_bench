#!/bin/bash

./script/runSpeculaBench.sh 4 0 70 false true 0 specula_tests
./script/runSpeculaBench.sh 4 0 70 false true 0 specula_tests
exit
./script/runSpeculaBench.sh 4 0 70 false true 0 specula_tests 2 10000 100
./script/runSpeculaBench.sh 4 0 70 false true 0 specula_tests 20 10000 100
./script/runSpeculaBench.sh 4 0 70 false true 0 specula_tests 10 1000 100
./script/runSpeculaBench.sh 4 0 70 false true 0 specula_tests 10 20000 100
./script/runSpeculaBench.sh 4 0 70 false true 0 specula_tests 10 30000 100
./script/runSpeculaBench.sh 4 0 70 false true 0 specula_tests 10 10000 50
./script/runSpeculaBench.sh 4 0 70 false true 0 specula_tests 10 10000 200
./script/runSpeculaBench.sh 4 0 70 false true 0 specula_tests 10 10000 300
exit
./script/runSpeculaBench.sh 4 80 4 true true 1 specula_tests
./script/runSpeculaBench.sh 4 80 4 true true 2 specula_tests
exit
## Only update slave 
#./script/runSpeculaBench.sh 1 0 40 true false 1 specula_tests
#./script/runSpeculaBench.sh 1 0 40 true false 2 specula_tests
#./script/runSpeculaBench.sh 1 0 40 true false 4 specula_tests
./script/runSpeculaBench.sh 1 0 40 true true 0 specula_tests
./script/runSpeculaBench.sh 1 0 40 true true 1 specula_tests
./script/runSpeculaBench.sh 1 0 40 true true 2 specula_tests
./script/runSpeculaBench.sh 1 0 40 true true 4 specula_tests
./script/runSpeculaBench.sh 1 0 40 false false 4 specula_tests

# Only update master
./script/runSpeculaBench.sh 1 40 0 true true 0 specula_tests
./script/runSpeculaBench.sh 1 40 0 true true 1 specula_tests
./script/runSpeculaBench.sh 1 40 0 true true 2 specula_tests
./script/runSpeculaBench.sh 1 40 0 true true 4 specula_tests
./script/runSpeculaBench.sh 1 40 0 false false 1 specula_tests

#Only update other 
#./script/runSpeculaBench.sh 1 0 0 true false 1 specula_tests
#./script/runSpeculaBench.sh 1 0 0 true false 2 specula_tests
#./script/runSpeculaBench.sh 1 0 0 true false 4 specula_tests
#./script/runSpeculaBench.sh 1 0 0 true true 1 specula_tests
#./script/runSpeculaBench.sh 1 0 0 true true 2 specula_tests
#./script/runSpeculaBench.sh 1 0 0 true true 4 specula_tests
#./script/runSpeculaBench.sh 1 0 0 false false 1 specula_tests

#Low locality 
#./script/runSpeculaBench.sh 1 0 20 true false 1 specula_tests
#./script/runSpeculaBench.sh 1 0 20 true false 2 specula_tests
#./script/runSpeculaBench.sh 1 0 20 true false 4 specula_tests
#./script/runSpeculaBench.sh 1 0 20 true true 1 specula_tests
#./script/runSpeculaBench.sh 1 0 20 true true 2 specula_tests
#./script/runSpeculaBench.sh 1 0 20 true true 4 specula_tests
#./script/runSpeculaBench.sh 1 0 20 false false 1 specula_tests

# High locality 
./script/runSpeculaBench.sh 1 80 4 true true 0 specula_tests
./script/runSpeculaBench.sh 1 80 4 true true 1 specula_tests
./script/runSpeculaBench.sh 1 80 4 true true 2 specula_tests
./script/runSpeculaBench.sh 1 80 4 true true 4 specula_tests
./script/runSpeculaBench.sh 1 80 4 false false 1 specula_tests

./script/runSpeculaBench.sh 4 80 4 true true 1 specula_tests
./script/runSpeculaBench.sh 4 80 4 true true 2 specula_tests
exit
