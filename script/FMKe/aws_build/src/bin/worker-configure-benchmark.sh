ANTIDOTE_NODES="${ANTIDOTE_NODES}"
BENCH_NODE="${BENCH_NODE}"

sed -ie "s#{antidote_nodes, .*}.#{antidote_nodes, ['${ANTIDOTE_NODES}']}.#g" ${REMOTE_CONFIG_FILE}
if [ "$?" = 0  ]; then
    echo "[SCRIPT]: Configured Antidote server addresses."
else
    echo "[SCRIPT]: Could not write Antidote server addresses to node ${IP_ADDR}, aborting..."
    exit 1
fi

sed -ie "s#{bench_node, .*}.#{bench_node, ['${BENCH_NODE}', longnames]}.#g" ${REMOTE_CONFIG_FILE}
if [ "$?" = 0  ]; then
    echo "[SCRIPT]: Configured bench node address."
else
    echo "[SCRIPT]: Could not write bench node address for node ${IP_ADDR}, aborting..."
    exit 1
fi

sed -ie 's#{concurrent, [0-9]\+}.#{concurrent, '"${NUM_CLIENTS}"'}.#g' ${REMOTE_CONFIG_FILE}
if [ "$?" = 0  ]; then
    echo "[SCRIPT]: Configured number of basho bench clients."
else
    echo "[SCRIPT]: Could not write number of basho bench clients in node ${IP_ADDR}, aborting..."
    exit 1
fi

sed -ie 's#{duration, [0-9]\+}.#{duration, '"${BENCHDURATION}"'}.#g' ${REMOTE_CONFIG_FILE}
if [ "$?" = 0  ]; then
    echo "[SCRIPT]: Configured benchmark duration."
else
    echo "[SCRIPT]: Could not write benchmark duration in node ${IP_ADDR}, aborting..."
    exit 1
fi
echo "[SCRIPT]: Node ${IP_ADDR} has been successfully configured."
