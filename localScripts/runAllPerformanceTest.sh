#!/bin/bash
set -e
File="speculaResults/performance"
./localScripts/runPerformanceTest.sh 1 10 $File 
./localScripts/runPerformanceTest.sh 6 10 $File 
./localScripts/runPerformanceTest.sh 1 100 $File 
./localScripts/runPerformanceTest.sh 6 100 $File 
./localScripts/runPerformanceTest.sh 1 500 $File 
./localScripts/runPerformanceTest.sh 6 500 $File 
./localScripts/runPerformanceTest.sh 1 1000 $File 
./localScripts/runPerformanceTest.sh 6 1000 $File 
