#!/bin/bash
# this script merges results from different runs
# it assumes the given folder contains folders with the results of different runs
# (where each result is the output of master-mergeResults.sh)
# the folder structure should look like this:
# $dir
#   run-1
#     summary
#   run-2
#     summary
#   ...
#   run-n
#     summary
#
# to run: merge-runs.sh <directory> ./merge-runs.sh <directory with runs> <basho_bench directory>
set -e

echo "Starting merge..."
HELP="Run it like this: ./merge-runs.sh <directory with runs> <basho_bench directory>"

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

# Define the number of bench nodes from the number of files in the directory
# NOTE: this assumes that the master-runBenchmarkStarter.sh script has already verified
# that all workers have sent their results to the target dir.
cd "$DIRECTORY"
NUM_RUNS=$(find . -mindepth 1 -maxdepth 1 -type d -iname "run-*" | wc -l)

# create the summary result if it does not exist
if [ ! -d "summary" ] ; then
    mkdir summary
fi

########################################################
    # Merge Summary Files
########################################################
echo "Merging summary files..."
SUMMARY_FILE=summary.csv
SUMMARY_FILES=""
for DIR in run-* ; do
    SUMMARY_FILES="${DIRECTORY}/${DIR}/summary/${SUMMARY_FILE} ${SUMMARY_FILES}"
done

awk -f "${BASHO_BENCH}/script/mergeResultsSummary.awk" $SUMMARY_FILES > "${DIRECTORY}/summary/${SUMMARY_FILE}"

########################################################
    # Merge Latency Files
########################################################
echo "Merging latency files..."
# we always assume there is a folder inside called run-1
cd run-1/summary

NUM_LATENCY_FILES=$(find . -mindepth 1 -maxdepth 1 -iname "*_latencies.csv" | wc -l)
for LATENCY_FILE in *_latencies.csv ; do
    cd "$DIRECTORY"
    LATENCY_FILES=""
    for RUN in run-* ; do
        LATENCY_FILES="${DIRECTORY}/${RUN}/summary/${LATENCY_FILE} ${LATENCY_FILES}"
    done

    awk -f "${BASHO_BENCH}/script/mergeResults.awk" $LATENCY_FILES > "${DIRECTORY}/summary/${LATENCY_FILE}"
done

cd "$DIRECTORY/summary"
echo "Merging newly created latency files into a single one (for throughput vs latency graphs)..."

LATENCY_FILES=""
for LATENCY_FILE in *_latencies.csv ; do
    LATENCY_FILES="${DIRECTORY}/summary/${LATENCY_FILE} ${LATENCY_FILES}"
done

awk -f "${BASHO_BENCH}/script/mergeResults.awk" $LATENCY_FILES > "${DIRECTORY}/summary/throughput_vs_latency.csv"
tail -n +2 "${DIRECTORY}/summary/throughput_vs_latency.csv" | awk '{split($0,a);print(a[3]/a[2]),a[5]/1}' | sort -n | tr ' ' ', ' > "${DIRECTORY}/summary/throughput_vs_latency_simple.csv"

echo "Building summary PNG..."
Rscript --vanilla "${BASHO_BENCH}/priv/summary.r" -i "$DIRECTORY/summary"

echo "Building throughput vs latency graph..."
gnuplot -e "constant=($NUM_LATENCY_FILES*$NUM_RUNS);inputname='${DIRECTORY}/summary/throughput_vs_latency_simple.csv'; outputname='${DIRECTORY}/summary/throughput_vs_latency.pdf'" "${BASHO_BENCH}/priv/throughput_vs_latency.gp"

echo "Done!"