#!/usr/bin/env bash
# This script assumes that there is an instance of FMKe already running.
# It is called by the master node through SSH, in: master-runBenchmarkStarter.sh
# on the local machine, and listenning to http requests on the 9097 port.
# That instance of FMKe should already be connected to target antidote nodes.

# INPUT:
# 1) THE IP ADDRESS OF THE MASTER NODE, USED TO SCP THE RESULTS
# 2) THE TESTNAME, TO SCP INTO THAT DIRECTORY AT THE MASTER NODE

if [ -z "$1" ]
  then
    MasterNodeIp="127.0.0.1"
  else
    MasterNodeIp=${1}
fi
echo "MasterNodeIp = ${MasterNodeIp}"

if [ -z "$2" ]
  then
    DateTime=`date +%Y-%m-%d-%H-%M-%S`
    BenchResultsDirectory="~/basho_bench/tests/fmk-bench-${DateTime}"
  else
    BenchResultsDirectory=${2}
fi
echo "Benchmark directoryName = ${BenchResultsDirectory}"

# Make the setup test executable
chmod +x ~/FMKe/test/fmk_setup_script.erl

# This is necessary when running on OS X, erlang 19
PATH="$PATH:/opt/local/lib/erlang/erts-8.1/bin/"
# Run the setup test
echo "cding into ~/FMKe"
cd ~/FMKe/
pwd
SetupCommand="~/FMKe/test/fmk_setup_script.erl 1 fmk@127.0.0.1"
echo "Running Setup script with command: "
echo "${SetupCommand}"
#eval ${SetupCommand}
# Run basho_bench
echo "cding into ~/basho_bench"
cd ../basho_bench
pwd
RunBenchCommand="~/basho_bench/_build/default/bin/basho_bench examples/fmkclient.config"
echo "Running Benchmark with command: "
echo "${RunBenchCommand}"
eval ${RunBenchCommand}
#	-Rscript --vanilla ${BENCH}/priv/summary.r -i tests/current
# Use the following line if one can obtain the public IP address of this machine from its adapter.
MY_IP=$(ifconfig en0 | grep inet | grep -v inet6 | awk '{print $2}')
# Otherwise, get the public IP
#MY_IP=$(dig +short myip.opendns.com @resolver1.opendns.com.)
# Tar the results
TarFileName=./test-"$MY_IP".tar
TarResultsCommand="tar cvzf ${TarFileName} tests/current"
echo "Running Benchmark with command: "
echo "${TarResultsCommand}"
eval ${TarResultsCommand}

# SCP THE RESULTS TO THE MASTER NODE
ScpResultsCommand="scp ${TarFileName} alek@${MasterNodeIp}:${BenchResultsDirectory}/"
echo "SCPing results to master node with command: "
echo "${ScpResultsCommand}"
eval ${ScpResultsCommand}
echo "worker ${MY_IP} done"

# the "master node" should collect this tar afterwards through scp.