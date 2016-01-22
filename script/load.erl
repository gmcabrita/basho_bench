-module(load).

-export([load_tpcc/1]).

load_tpcc(StringNodes) ->
    Cookie = antidote,
    true = erlang:set_cookie(node(), Cookie),
    [Node, Str2, Str3] = StringNodes,
    AntNode = list_to_atom("antidote@"++Node),
    Type = list_to_atom(Str2), 
    Param = list_to_integer(Str3), 
    io:format("Asking ~w to load ~w, Param is ~w", [AntNode, Type, Param]),
    rpc:call(AntNode, tx_cert_sup, load, [Type, Param]).
