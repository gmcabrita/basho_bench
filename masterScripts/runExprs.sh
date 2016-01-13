#!/bin/bash

## Just to test.. 
./script/runSpeculaBench.sh 4 70 20 true true 1 specula_tests
./script/runSpeculaBench.sh 4 70 20 true true 2 specula_tests
./script/runSpeculaBench.sh 4 70 20 true true 4 specula_tests
./script/runSpeculaBench.sh 4 70 20 true false 1 specula_tests
./script/runSpeculaBench.sh 4 70 20 true false 2 specula_tests
./script/runSpeculaBench.sh 4 70 20 true false 4 specula_tests
./script/runSpeculaBench.sh 1 70 20 false false 0 specula_tests
./script/runSpeculaBench.sh 2 70 20 false false 0 specula_tests
./script/runSpeculaBench.sh 4 70 20 false false 0 specula_tests
./script/runSpeculaBench.sh 1 70 20 true false 0 specula_tests
./script/runSpeculaBench.sh 1 70 20 true false 1 specula_tests
./script/runSpeculaBench.sh 1 70 20 true false 2 specula_tests
./script/runSpeculaBench.sh 1 70 20 true false 4 specula_tests
./script/runSpeculaBench.sh 1 70 20 true true 1 specula_tests
./script/runSpeculaBench.sh 1 70 20 true true 2 specula_tests
./script/runSpeculaBench.sh 1 70 20 true true 4 specula_tests
./script/runSpeculaBench.sh 2 70 20 true false 1 specula_tests
./script/runSpeculaBench.sh 2 70 20 true false 2 specula_tests
./script/runSpeculaBench.sh 2 70 20 true false 4 specula_tests
./script/runSpeculaBench.sh 2 70 20 true true 1 specula_tests
./script/runSpeculaBench.sh 2 70 20 true true 2 specula_tests
./script/runSpeculaBench.sh 2 70 20 true true 4 specula_tests
exit
#./script/runSpeculaBench.sh 1 100 0 false false 0 specula_tests
#./script/runSpeculaBench.sh 1 100 0 false false 0 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 0 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 0 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 0 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 0 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 0 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 0 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 1 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 1 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 1 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 1 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 1 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 1 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 2 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 2 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 2 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 2 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 2 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 2 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 4 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 4 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 4 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 4 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 4 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 4 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 8 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 8 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 8 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 8 specula_tests

exit

./script/runSpeculaBench.sh 1 0 100 false false 0 specula_tests
./script/runSpeculaBench.sh 2 0 100 false false 0 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true false 5 specula_tests
#./script/runSpeculaBench.sh 1 100 0 true true 5 specula_tests
#./script/runSpeculaBench.sh 2 100 0 true false 5 specula_tests
#./script/runSpeculaBench.sh 2 100 0 true true 5 specula_tests
./script/runSpeculaBench.sh 1 0 100 true false 5 specula_tests
./script/runSpeculaBench.sh 1 0 100 true false 5 specula_tests
./script/runSpeculaBench.sh 1 0 100 true true 5 specula_tests
./script/runSpeculaBench.sh 1 0 100 true true 5 specula_tests
./script/runSpeculaBench.sh 2 0 100 true false 5 specula_tests
./script/runSpeculaBench.sh 2 0 100 true false 5 specula_tests
./script/runSpeculaBench.sh 2 0 100 true true 5 specula_tests
./script/runSpeculaBench.sh 2 0 100 true true 5 specula_tests
exit

# Only update replica 
./script/runSpeculaBench.sh 1 0 100 true 0 specula_tests
./script/runSpeculaBench.sh 1 0 100 true 0 specula_tests
./script/runSpeculaBench.sh 1 0 100 true 0 specula_tests
./script/runSpeculaBench.sh 1 0 100 true 1 specula_tests
./script/runSpeculaBench.sh 1 0 100 true 1 specula_tests
./script/runSpeculaBench.sh 1 0 100 true 1 specula_tests
./script/runSpeculaBench.sh 1 0 100 true 2 specula_tests
./script/runSpeculaBench.sh 1 0 100 true 2 specula_tests
./script/runSpeculaBench.sh 1 0 100 true 2 specula_tests
./script/runSpeculaBench.sh 1 0 100 true 4 specula_tests
./script/runSpeculaBench.sh 1 0 100 true 4 specula_tests
./script/runSpeculaBench.sh 1 0 100 true 4 specula_tests
./script/runSpeculaBench.sh 1 0 100 false 1 specula_tests
./script/runSpeculaBench.sh 1 0 100 false 1 specula_tests
./script/runSpeculaBench.sh 1 0 100 false 1 specula_tests

#Only update other 
#./script/runSpeculaBench.sh 1 0 0 true 1 specula_tests
#./script/runSpeculaBench.sh 1 0 0 true 2 specula_tests
#./script/runSpeculaBench.sh 1 0 0 true 4 specula_tests
#./script/runSpeculaBench.sh 1 0 0 true 1 specula_tests
#./script/runSpeculaBench.sh 1 0 0 true 2 specula_tests
#./script/runSpeculaBench.sh 1 0 0 true 4 specula_tests
#./script/runSpeculaBench.sh 1 0 0 false 1 specula_tests

#Low locality 
#./script/runSpeculaBench.sh 1 0 20 true 1 specula_tests
#./script/runSpeculaBench.sh 1 0 20 true 2 specula_tests
#./script/runSpeculaBench.sh 1 0 20 true 4 specula_tests
#./script/runSpeculaBench.sh 1 0 20 true 1 specula_tests
#./script/runSpeculaBench.sh 1 0 20 true 2 specula_tests
#./script/runSpeculaBench.sh 1 0 20 true 4 specula_tests
#./script/runSpeculaBench.sh 1 0 20 false 1 specula_tests

# High locality 
#./script/runSpeculaBench.sh 1 80 4 true 0 specula_tests
#./script/runSpeculaBench.sh 1 80 4 true 1 specula_tests
#./script/runSpeculaBench.sh 1 80 4 true 2 specula_tests
#./script/runSpeculaBench.sh 1 80 4 true 4 specula_tests
#./script/runSpeculaBench.sh 1 80 4 false 1 specula_tests

#./script/runSpeculaBench.sh 4 80 4 true 1 specula_tests
#./script/runSpeculaBench.sh 4 80 4 true 2 specula_tests
exit
