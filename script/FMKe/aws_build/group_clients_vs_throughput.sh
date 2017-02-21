#!/bin/bash
# to run: ./group_clients_vs_throughput_and_latency.sh <directory with results> <directory with results 2> <basho_bench directory> <title one> <title two>
set -e

echo "Starting merge..."
HELP="Run it like this: ./group_clients_vs_throughput_and_latency.sh <directory with results> <directory with results 2> <basho_bench directory> <title one> <title two>"

if [ -z "$1" ] ; then
    echo "[ABORT] $HELP"
    exit 1
fi

if [ -z "$2" ] ; then
    echo "[ABORT] ${HELP}"
    exit 1
fi

if [ -z "$3" ] ; then
    echo "[ABORT] ${HELP}"
    exit 1
fi
if [ -z "$4" ] ; then
    echo "[ABORT] ${HELP}"
    exit 1
fi

if [ -z "$5" ] ; then
    echo "[ABORT] ${HELP}"
    exit 1
fi

DIRECTORY_ONE=$(realpath "$1")
DIRECTORY_TWO=$(realpath "$2")
BASHO_BENCH=$(realpath "$3")

if [ ! -d "$DIRECTORY_ONE" ] ; then
    echo "[ABORT] Directory does not exist!"
    exit 1
fi

if [ ! -d "$DIRECTORY_TWO" ] ; then
    echo "[ABORT] Directory does not exist!"
    exit 1
fi

if [ ! -d "$BASHO_BENCH" ] ; then
    echo "[ABORT] Basho bench directory does not exist!"
    exit 1
fi

echo "Building clients vs throughput and latency graphs..."
gnuplot -e "titleone='$4'; titletwo='$5'; inputone='${DIRECTORY_ONE}/clients_throughput_latency_sorted.csv'; inputtwo='${DIRECTORY_TWO}/clients_throughput_latency_sorted.csv'; outputname='clients_vs_throughput.pdf'" "${BASHO_BENCH}/priv/clients_vs_throughput_grouped.gp"
gnuplot -e "titleone='$4'; titletwo='$5'; inputone='${DIRECTORY_ONE}/clients_throughput_latency_sorted.csv'; inputtwo='${DIRECTORY_TWO}/clients_throughput_latency_sorted.csv'; outputname='clients_vs_latency.pdf'" "${BASHO_BENCH}/priv/clients_vs_latency_grouped.gp"

echo "Done!"
