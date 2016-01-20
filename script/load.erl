-module(load).

-export([load_tpcc/1]).

load_tpcc(StringNodes) ->
    Cookie = antidote,
    true = erlang:set_cookie(node(), Cookie),
    [Node, Str2] = StringNodes,
    AntNode = list_to_atom("antidote@"++Node),
    WPerDc = list_to_integer(Str2), 
    io:format("Asking ~w to load ~w warehouses", [AntNode, WPerDc]),
    rpc:call(AntNode, tx_cert_sup, load_tpcc, [WPerDc]).
