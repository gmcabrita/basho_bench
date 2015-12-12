#!/bin/bash

./script/runSpeculaBench.sh 1 100 0 true false 1 specula_tests
./script/runSpeculaBench.sh 1 100 0 true false 5 specula_tests
./script/runSpeculaBench.sh 1 100 0 true false 10 specula_tests
./script/runSpeculaBench.sh 1 100 0 true true 1 specula_tests
./script/runSpeculaBench.sh 1 100 0 true true 5 specula_tests
./script/runSpeculaBench.sh 1 100 0 true true 10 specula_tests

./script/runSpeculaBench.sh 1 100 0 false false 1 specula_tests



