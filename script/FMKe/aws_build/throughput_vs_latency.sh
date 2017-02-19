#!/bin/bash
# this script generates the throughput vs latency graphs for one run
# it assumes the given folder contains folders with the results of a single run
# as such, it should be run after ./7-fetch-and-merge-results.sh
#
# the folder structure should look like this:
# $benchmark
#   results-1
#   results-2
#   ...
#   results-n
#   summary
#
# to run: ./throughput_vs_latency.sh <directory with results> <basho_bench directory>
set -e

echo "Starting merge..."
HELP="Run it like this: ./throughput_vs_latency.sh <directory with results> <basho_bench directory>"

if [ -z "$1" ] ; then
    echo "[ABORT] $HELP"
    exit 1
fi

if [ -z "$2" ] ; then
    echo "[ABORT] ${HELP}"
    exit 1
fi

DIRECTORY=$(realpath "$1")
BASHO_BENCH=$(realpath "$2")

if [ ! -d "$DIRECTORY" ] ; then
    echo "[ABORT] Directory does not exist!"
    exit 1
fi

if [ ! -d "$BASHO_BENCH" ] ; then
    echo "[ABORT] Basho bench directory does not exist!"
    exit 1
fi

# Define the number of bench nodes from the number of files in the directory
# NOTE: this assumes that the master-runBenchmarkStarter.sh script has already verified
# that all workers have sent their results to the target dir.
cd "$DIRECTORY"
NUM_RUNS=$(find . -mindepth 1 -maxdepth 1 -type d -iname "results-*" | wc -l)

########################################################
    # Merge Latency Files
########################################################
echo "Merging latency files (if there is more than one)..."
# we always assume there is a folder inside called summary
cd summary

NUM_LATENCY_FILES=$(find . -mindepth 1 -maxdepth 1 -iname "*_latencies.csv" | wc -l)
LATENCY_FILES=""
for LATENCY_FILE in *_latencies.csv ; do
    LATENCY_FILES="${DIRECTORY}/summary/${LATENCY_FILE} ${LATENCY_FILES}"
done

awk -f "${BASHO_BENCH}/script/mergeResults.awk" $LATENCY_FILES > "${DIRECTORY}/summary/throughput_vs_latency.csv"
tail -n +2 "${DIRECTORY}/summary/throughput_vs_latency.csv" | awk '{split($0,a);print(a[3]/a[2]),a[5]/1}' | sort -n | tr ' ' ', ' > "${DIRECTORY}/summary/throughput_vs_latency_simple.csv"

echo "Building throughput vs latency graph..."
gnuplot -e "constant=($NUM_LATENCY_FILES*$NUM_RUNS);inputname='${DIRECTORY}/summary/throughput_vs_latency_simple.csv'; outputname='${DIRECTORY}/summary/throughput_vs_latency.pdf'" "${BASHO_BENCH}/priv/throughput_vs_latency.gp"

echo "Done!"