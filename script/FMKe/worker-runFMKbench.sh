#!/usr/bin/env bash
# This script assumes that there is an instance of FMKe already running.
# It is called by the master node through SSH, in: master-runBenchmarkStarter.sh
# on the local machine, and listenning to http requests on the 9097 port.
# That instance of FMKe should already be connected to target antidote nodes.

# run like : MasterNodeIp=1.2.3.4 BenchResultsDirectory=dir RUNFMKSETUP=TRUE/FALSE worker-runFMKbench.sh
# INPUT:
# 1) THE IP ADDRESS OF THE MASTER NODE, USED TO SCP THE RESULTS
# 2) THE TESTNAME, TO SCP INTO THAT DIRECTORY AT THE MASTER NODE
# 3) RUNFMKSETUP=TRUE/FALSE

# Use the following line if one can obtain the public IP address of this machine from its adapter.
    MY_IP=$(ifconfig en0 | grep inet | grep -v inet6 | awk '{print $2}')
    # Otherwise, get the public IP
    #MY_IP=$(dig +short myip.opendns.com @resolver1.opendns.com.)
    
if [ -z "$MasterNodeIp" ]
  then
    MasterNodeIp="127.0.0.1"
fi
echo "##Node:${MY_IP}: MasterNodeIp = ${MasterNodeIp}"

if [ -z "$BenchResultsDirectory" ]
  then
    DateTime=`date +%Y-%m-%d-%H-%M-%S`
    BenchResultsDirectory="~/basho_bench/tests/fmk-bench-${DateTime}"
fi
echo "##Node:${MY_IP}: Benchmark directoryName = ${BenchResultsDirectory}"

# Make the setup test executable
chmod +x ~/FMKe/test/fmk_setup_script.erl

# This is necessary when running on OS X, erlang 19
PATH="$PATH:/opt/local/lib/erlang/erts-8.1/bin/"

# first check that fmk is active on that node:
FmkPing=$(~/basho_bench/script/FMKe/ping.erl 'fmk@127.0.0.1')
echo "##Node:${MY_IP}:Pinging 'fmk@127.0.0.1', got: ${FmkPing}"
if [ "$FmkPing" = pong ] ; then
    # Run the setup test
    if [ "$RUNFMKSETUP" = TRUE ] ; then
        echo "##Node:${MY_IP}: cding into ~/FMKe"
        cd ~/FMKe/
        pwd
        SetupCommand="~/FMKe/test/fmk_setup_script.erl 1 fmk@127.0.0.1"
        echo "##Node:${MY_IP}: Running Setup script with command: "
        echo "##Node:${MY_IP}: ${SetupCommand}"
        eval ${SetupCommand}
    else
        echo "##Node:${MY_IP}: not running fmk setup."

    fi

    # Run basho_bench
    echo "##Node:${MY_IP}: cding into ~/basho_bench"
    cd ~/basho_bench
    pwd
    RunBenchCommand="~/basho_bench/_build/default/bin/basho_bench examples/fmkclient.config"
    echo "##Node:${MY_IP}: Running Benchmark with command: "
    echo "##Node:${MY_IP}: ${RunBenchCommand}"
    eval ${RunBenchCommand}
    #	-Rscript --vanilla ${BENCH}/priv/summary.r -i tests/current
    # Tar the results
    TarFileName=./test-"$MY_IP".tar
    TarResultsCommand="tar cvzf ${TarFileName} tests/current"
    echo "##Node:${MY_IP}: Running Benchmark with command: "
    echo "##Node:${MY_IP}: ${TarResultsCommand}"
    eval ${TarResultsCommand}

    # SCP THE RESULTS TO THE MASTER NODE
    ScpResultsCommand="scp -o StrictHostKeyChecking=no ${TarFileName} alek@${MasterNodeIp}:${BenchResultsDirectory}/"
    echo "##Node:${MY_IP}: SCPing results to master node with command: "
    echo "##Node:${MY_IP}: ${ScpResultsCommand}"
    eval ${ScpResultsCommand}
    # the "master node" should collect this tar afterwards through scp.
else
    echo "##Node:${MY_IP}: fmk is not running on worker ${MY_IP}, nothing to do..."
fi
echo "##Node:${MY_IP}: worker ${MY_IP} done."




