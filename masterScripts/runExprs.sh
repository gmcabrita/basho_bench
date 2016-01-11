#!/bin/bash

## Just to test.. 
./script/runSpeculaBench.sh 1 70 25 false 0 specula_tests
./script/runSpeculaBench.sh 1 70 25 false 0 specula_tests
./script/runSpeculaBench.sh 1 70 25 false 0 specula_tests
./script/runSpeculaBench.sh 1 70 25 false 1 specula_tests
./script/runSpeculaBench.sh 1 70 25 false 1 specula_tests
./script/runSpeculaBench.sh 1 70 25 false 1 specula_tests
./script/runSpeculaBench.sh 1 70 25 false 2 specula_tests
./script/runSpeculaBench.sh 1 70 25 false 2 specula_tests
./script/runSpeculaBench.sh 1 70 25 false 2 specula_tests

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
