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

########################################################
    # get my IP address
##########################################################

# Use the following line if one can obtain the public IP address of this machine from its adapter.
    
if [ -z "$Worker_IP" ]; then
    Worker_IP=$(ifconfig en0 | grep inet | grep -v inet6 | awk '{print $2}')
    # Otherwise, get the public IP
    #Worker_IP=$(dig +short myip.opendns.com @resolver1.opendns.com.)
fi


########################################################
    # Obtain the input variables
##########################################################
if [ -z "$MasterNodeIp" ]
  then
    MasterNodeIp="127.0.0.1"
fi
echo "##Node:${Worker_IP}: MasterNodeIp = ${MasterNodeIp}"

if [ -z "$BenchResultsDirectory" ]
  then
    DateTime=`date +%Y-%m-%d-%H-%M-%S`
    BenchResultsDirectory="~/basho_bench/tests/fmk-bench-${DateTime}"
fi
echo "##Node:${Worker_IP}: Benchmark directoryName = ${BenchResultsDirectory}"

########################################################
    # Run (or not) the FMKe setup script
##########################################################
# Make the setup test executable
chmod +x ~/FMKe/test/fmk_setup_script.erl

# This is only necessary when running on OS X, erlang 19
# might be removed, but won't harm otherwise...
PATH="$PATH:/opt/local/lib/erlang/erts-8.1/bin/"

# first check that fmk is active (pingable) on that node:
FmkPing=$(~/basho_bench/script/FMKe/ping.erl 'fmk@127.0.0.1')
echo "##Node:${Worker_IP}:Pinging 'fmk@127.0.0.1', got: ${FmkPing}"
if [ "$FmkPing" = pong ] ; then
    # Run the setup test
    if [ "$RUNFMKSETUP" = TRUE ] ; then
        echo "##Node:${Worker_IP}: cding into ~/FMKe"
        cd ~/FMKe/
        pwd
        SetupCommand="~/FMKe/test/fmk_setup_script.erl 1 fmk@127.0.0.1"
        echo "##Node:${Worker_IP}: Running Setup script with command: "
        echo "##Node:${Worker_IP}: ${SetupCommand}"
        eval ${SetupCommand}
    else
        echo "##Node:${Worker_IP}: not running fmk setup."

    fi

########################################################
    # Run basho_bench
##########################################################
    echo "##Node:${Worker_IP}: cding into ~/basho_bench"
    cd ~/basho_bench
    pwd
    RunBenchCommand="~/basho_bench/_build/default/bin/basho_bench examples/fmkclient.config"
    echo "##Node:${Worker_IP}: Running Benchmark with command: "
    echo "##Node:${Worker_IP}: ${RunBenchCommand}"
    eval ${RunBenchCommand}

########################################################
    # Tar the results
##########################################################
    TarFileName=./test-"$Worker_IP".tar
    TarResultsCommand="tar cvzf ${TarFileName} tests/current"
    echo "##Node:${Worker_IP}: Running Benchmark with command: "
    echo "##Node:${Worker_IP}: ${TarResultsCommand}"
    eval ${TarResultsCommand}

########################################################
    # SCP the results to the master node, into the BenchResultsDirectory
##########################################################
    ScpResultsCommand="scp -o StrictHostKeyChecking=no ${TarFileName} alek@${MasterNodeIp}:${BenchResultsDirectory}/"
    echo "##Node:${Worker_IP}: SCPing results to master node with command: "
    echo "##Node:${Worker_IP}: ${ScpResultsCommand}"
    eval ${ScpResultsCommand}
    # the "master node" should collect this tar afterwards through scp.
else
    echo "##Node:${Worker_IP}: fmk is not running on worker ${Worker_IP}, nothing to do..."
fi
echo "##Node:${Worker_IP}: worker ${Worker_IP} done."




