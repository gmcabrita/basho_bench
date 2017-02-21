#!/bin/bash
# this script generates the throughput vs latency graphs for one run
# it assumes the given folder contains folders with the results of a single run
# as such, it should be run after ./7-fetch-and-merge-results.sh
#
# the folder structure should look like this:
# $benchmark
#   1
#     run-1
#     run-2
#     run-2
#   2
#     ...
#   4
#     ...
#   ...
#   32
#     ...
#
# to run: ./clients_vs_throughput_and_latency.sh <directory with results> <basho_bench directory>
set -e

echo "Starting merge..."
HELP="Run it like this: ./clients_vs_throughput_and_latency.sh <directory with results> <basho_bench directory>"

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

cd "$DIRECTORY"

rm -f clients_throughput_latency.csv
rm -f clients_throughput_latency_sorted.csv

echo "Merging all runs for each collection."
for DIR in */ ; do
    ./../merge-runs.sh "$DIR" "$BASHO_BENCH" ;
done

echo "Generating latency files for each collection."
for DIR in */ ; do
    ./../throughput_vs_latency.sh "$DIR" "$BASHO_BENCH" ;
done


echo "Building clients per dc vs mean throughput vs mean latency CSV"
touch clients_throughput_latency.csv

for DIR in */ ; do
    cd "${DIRECTORY}/${DIR}/summary"
    NUM_LATENCY_FILES=$(find . -mindepth 1 -maxdepth 1 -iname "*_latencies.csv" | wc -l)
    NUM_BENCH_NODES=5 # assuming this was run on 5 basho bench nodes
    MEANS=$(cat "${DIRECTORY}/${DIR}/summary/throughput_vs_latency_simple.csv" | tr ',' ' ' | awk '{ throughput += $1; latency += $2 } END { print (throughput/NR)*'"($NUM_BENCH_NODES*$NUM_LATENCY_FILES)"', latency/NR }')
    echo $MEANS
    echo "$(basename ${DIR}) ${MEANS}" | tr ' ' ', ' >> "${DIRECTORY}/clients_throughput_latency.csv"
done

sort -n "${DIRECTORY}/clients_throughput_latency.csv" > "${DIRECTORY}/clients_throughput_latency_sorted.csv"

echo "Building clients vs throughput and latency graphs..."
gnuplot -e "inputname='${DIRECTORY}/clients_throughput_latency_sorted.csv'; outputname='${DIRECTORY}/clients_vs_throughput.pdf'" "${BASHO_BENCH}/priv/clients_vs_throughput.gp"
gnuplot -e "inputname='${DIRECTORY}/clients_throughput_latency_sorted.csv'; outputname='${DIRECTORY}/clients_vs_latency.pdf'" "${BASHO_BENCH}/priv/clients_vs_latency.gp"

echo "Done!"
