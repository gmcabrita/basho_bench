#!/usr/bin/env bash
# This script assumes that there are tar files sent by the workers
# in a results/fmk-bench-date-time folder.
# It merges all those results into a single one.
# It is called by the master-runBenchmarkStarter.sh script

# run like : BenchResultsDirectory=$dir master-mergeResults.sh
# INPUT:
# 1) BenchResultsDirectory, the directory where the worker result tar files are stored.

########################################################
    # check we got a correct directory with tar files
#########################################################
if [ -z "$BenchResultsDirectory" ]
  then
    echo "Input params error, run like : BenchResultsDirectory=dir master-mergeResults.sh"
    exit 255
fi
echo "---### MASTER: STARTING to merge Results in ${BenchResultsDirectory}"

# Define the number of bench nodes from the number of files in the directory
# NOTE: this assumes that the master-runBenchmarkStarter.sh script has already verified
# that all workers have sent their results to the target dir.
cd $BenchResultsDirectory
Numfiles=$(eval "\ls -afq | wc -l")
# substract 2 as the previous command counts the . and .. directories
NumBenchNodes=$((Numfiles-2))

########################################################
    # Untar files into a dir with the tarfile name
#########################################################
set +f
for File in $(find . -maxdepth 1 -type f -name "*.tar.gz") ; do
        FileWithoutExtension="${File%.*.*}"
        echo "---### MASTER: Untaring file ${File} into directory ${FileWithoutExtension}"
        mkdir $FileWithoutExtension
        tar -C $FileWithoutExtension -xf "$File"
        rm $File
done

# create the summary result if it does not exist
if [ ! -d "summary" ]; then
    mkdir summary
    echo "---### MASTER: created summary directory"
else
    echo "summary directory already exists!"
fi

########################################################
    # Merge Summary Files
########################################################
# get all the directories of the untared files (they start with "results-")
for Dir in results-* ; do
        echo "---### MASTER: cding into $Dir/results/current/ to meet all the files we need to merge"
        cd $Dir/tests/current/ ## THIS IS CORRECT, DONT CHANGE!

        SummaryFile=summary.csv
        AllFilesWithThisName=""
        echo "---### MASTER: Collecting all ${SummaryFile} in $BenchResultsDirectory"
#       echo "---### MASTER: cding back into $BenchResultsDirectory"
        cd $BenchResultsDirectory
        for TestDir in results-* ; do
            AllFilesWithThisName=""$BenchResultsDirectory"/"$TestDir"/tests/current/${SummaryFile} "$AllFilesWithThisName""
        done
        echo "---### MASTER: all files with this name are: ${AllFilesWithThisName}"
        ########################################################
        # Now use this magic command to merge them into a file into the summary directory
        ########################################################
        echo "---### MASTER: Merging all those files into summary/${SummaryFile}"
        awk -f ../../../mergeResultsSummary.awk $AllFilesWithThisName > $BenchResultsDirectory/summary/${SummaryFile}
        echo "---### MASTER: done"

        ## We only needed the first directory to know which files we need to process,
        ## and, again, I suck at bash scripting...
        break
done



########################################################
    # Merge Latency Files
########################################################
# get all the directories of the untared files (they start with "results-")
for Dir in results-* ; do
        echo "---### MASTER: cding into $Dir/results/current/ to meet all the files we need to merge"
        cd $Dir/tests/current/ ## THIS IS CORRECT, DONT CHANGE!

        ########################################################
        # get all the latency files (that end with _latencies.csv") in the results directory
        ########################################################
        for LatencyFile in *_latencies.csv ; do
            AllFilesWithThisName=""
            echo "---### MASTER: Collecting all ${LatencyFile} in $BenchResultsDirectory"
#            echo "---### MASTER: cding back into $BenchResultsDirectory"
            cd $BenchResultsDirectory
            for TestDir in results-* ; do
                AllFilesWithThisName=""$BenchResultsDirectory"/"$TestDir"/tests/current/${LatencyFile} "$AllFilesWithThisName""
            done
            echo "---### MASTER: all files with this name are: ${AllFilesWithThisName}"
        ########################################################
        # Now use this magic command to merge them into a file into the summary directory
        ########################################################
            echo "---### MASTER: Merging all those files into summary/${LatencyFile}"
            awk -f ../../../mergeResults.awk $AllFilesWithThisName > $BenchResultsDirectory/summary/${LatencyFile}
            echo "---### MASTER: done"

        done
        ## We only needed the first directory to know which files we need to process,
        ## and I suck at bash scripting...
        break
done
