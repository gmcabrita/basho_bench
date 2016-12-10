#!/usr/bin/env bash
# This script assumes that there is an instance of FMKe already running.
# It is called by the master node through SSH, in: master-runBenchmarkStarter.sh
# on the local machine, and listenning to http requests on the 9097 port.
# That instance of FMKe should already be connected to target antidote nodes.

# Make the setup test executable
chmod +x ~/FMKe/test/fmk_setup_script.erl
# Run the setup test
echo "cding into ~/FMKe"
cd ~/FMKe/
pwd
SetupCommand="~/FMKe/test/fmk_setup_script.erl 1 fmk@127.0.0.1"
echo "Running Setup script with command: "
echo "${SetupCommand}"
eval ${SetupCommand}
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
TarResultsCommand="tar cvzf ./test-"$MY_IP".tar tests/current"
echo "Running Benchmark with command: "
echo "${TarResultsCommand}"
eval ${TarResultsCommand}

# the "master node" should collect this tar afterwards through scp.