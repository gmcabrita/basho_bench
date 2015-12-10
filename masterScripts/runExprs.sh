#!/bin/bash

./script/runSpeculaBench.sh 1 60 20 true specula_tests
./script/runSpeculaBench.sh 1 60 20 false specula_tests
./script/runSpeculaBench.sh 1 100 0 true specula_tests
./script/runSpeculaBench.sh 1 100 0 false specula_tests
