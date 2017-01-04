#!/usr/bin/env bash
# author goncalotomas
set -e

BENCHMARKS_DIR=${PWD}/benchmarks
PRIVATEKEY=$1
KEY_FILE_NAME=$(basename $PRIVATEKEY)
IP_ADDR_LIST=$(echo $* | cut -d' ' -f2-)
SSH_OPTIONS="-o StrictHostKeyChecking=no -i $PRIVATEKEY"
USER=ubuntu

if [ ! -d "$BENCHMARKS_DIR" ]; then
    mkdir $BENCHMARKS_DIR
fi

IP_ARR=(`echo ${IP_ADDR_LIST}`);

WORKER_SCRIPT="${PWD}/src/bin/compile-and-compress-results.sh"
REMOTE_RESULTS="/home/ubuntu/compile-and-compress-results.sh"

cd $BENCHMARKS_DIR

for index in "${!IP_ARR[@]}"; do
    ## FETCH TO LOCAL dir
    echo "[SCRIPT]: Copying results from remote machine..."
    WORKERID=$(($index + 1))
    FILENAME="results-${WORKERID}.tar.gz"
    scp ${SSH_OPTIONS} ${USER}@${IP_ARR[$index]}:${FILENAME} .
    echo "[SCRIPT]: Copied results from worker ${WORKERID}."
done

BASHO_BENCH_DIR=".."
BenchResultsDirectory=$BENCHMARKS_DIR
cd ..
####################################################
# Merge results in the test directory into a single one and create the results file image TODO
####################################################
#Call the merge results script
CommandToRunMergeScript="BenchResultsDirectory=$BenchResultsDirectory ~/git/basho_bench/script/FMKe/master-mergeResults.sh"
echo "[SCRIPT]: Calling merge script..."
eval $CommandToRunMergeScript

# Create an image with the summary
CommandToBuildPng="Rscript --vanilla priv/summary.r -i $BenchResultsDirectory/summary"
echo "--##--Master ${MY_IP}: Processing results into a summary.png file..."
echo "--##--Master ${MY_IP}: $CommandToBuildPng"
cd $BASHO_BENCH_DIR/../../
eval $CommandToBuildPng
echo "--##--Master ${MY_IP}: DONE, see your results!!!"
