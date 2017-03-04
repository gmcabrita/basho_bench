#!/bin/bash
# to run: ./group_4_clients_vs_throughput_and_latency.sh <results 1> <results 2> <results 3> <results 4> <basho_bench directory> <title 1> <title 2> <title 3> <title 4>
set -e

echo "Starting merge..."
HELP="Run it like this: ./group_4_clients_vs_throughput_and_latency.sh <results 1> <results 2> <results 3> <results 4> <basho_bench directory> <title 1> <title 2> <title 3> <title 4>"

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

if [ -z "$6" ] ; then
    echo "[ABORT] ${HELP}"
    exit 1
fi

if [ -z "$7" ] ; then
    echo "[ABORT] ${HELP}"
    exit 1
fi

if [ -z "$8" ] ; then
    echo "[ABORT] ${HELP}"
    exit 1
fi

if [ -z "$9" ] ; then
    echo "[ABORT] ${HELP}"
    exit 1
fi

DIRECTORY_ONE=$(realpath "$1")
DIRECTORY_TWO=$(realpath "$2")
DIRECTORY_THREE=$(realpath "$3")
DIRECTORY_FOUR=$(realpath "$4")
BASHO_BENCH=$(realpath "$5")

if [ ! -d "$DIRECTORY_ONE" ] ; then
    echo "[ABORT] Directory does not exist!"
    exit 1
fi

if [ ! -d "$DIRECTORY_TWO" ] ; then
    echo "[ABORT] Directory does not exist!"
    exit 1
fi

if [ ! -d "$DIRECTORY_THREE" ] ; then
    echo "[ABORT] Directory does not exist!"
    exit 1
fi

if [ ! -d "$DIRECTORY_FOUR" ] ; then
    echo "[ABORT] Directory does not exist!"
    exit 1
fi

if [ ! -d "$BASHO_BENCH" ] ; then
    echo "[ABORT] Basho bench directory does not exist!"
    exit 1
fi

echo "Building clients vs throughput and latency graphs..."
gnuplot -e "titleone='$6'; titletwo='$7'; titlethree='$8'; titlefour='$9'; inputone='${DIRECTORY_ONE}/clients_throughput_latency_sorted.csv'; inputtwo='${DIRECTORY_TWO}/clients_throughput_latency_sorted.csv'; inputthree='${DIRECTORY_THREE}/clients_throughput_latency_sorted.csv'; inputfour='${DIRECTORY_FOUR}/clients_throughput_latency_sorted.csv'; outputname='clients_vs_throughput.pdf'" "${BASHO_BENCH}/priv/clients_vs_throughput_grouped_several.gp"
gnuplot -e "titleone='$6'; titletwo='$7'; titlethree='$8'; titlefour='$9'; inputone='${DIRECTORY_ONE}/clients_throughput_latency_sorted.csv'; inputtwo='${DIRECTORY_TWO}/clients_throughput_latency_sorted.csv'; inputthree='${DIRECTORY_THREE}/clients_throughput_latency_sorted.csv'; inputfour='${DIRECTORY_FOUR}/clients_throughput_latency_sorted.csv'; outputname='clients_vs_latency.pdf'" "${BASHO_BENCH}/priv/clients_vs_latency_grouped_several.gp"

echo "Building clients vs throughput and latency graphs..."
gnuplot -e "titleone='$6'; titletwo='$7'; titlethree='$8'; titlefour='$9'; inputone='${DIRECTORY_ONE}/clients_throughput_latency_sorted.csv'; inputtwo='${DIRECTORY_TWO}/clients_throughput_latency_sorted.csv'; inputthree='${DIRECTORY_THREE}/clients_throughput_latency_sorted.csv'; inputfour='${DIRECTORY_FOUR}/clients_throughput_latency_sorted.csv'; outputname='throughput_vs_latency.pdf'" "${BASHO_BENCH}/priv/throughput_vs_latency_grouped_several.gp"

echo "Done!"
