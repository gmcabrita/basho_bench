-module(clean_data).

-export([clean_data/1]).

clean_data(StringNodes) ->
    Cookie = antidote,
    true = erlang:set_cookie(node(), Cookie),
    [Node] = StringNodes,
    AntNode = list_to_atom("antidote@"++Node),
    io:format("Sending clean data to ~w", [AntNode]),
    rpc:call(AntNode, tx_cert_sup, clean_all_data, []).
