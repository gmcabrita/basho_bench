-module(getStat).

-export([get_stat/1]).

get_stat(StringNodes) ->
    Cookie = antidote,
    true = erlang:set_cookie(node(), Cookie),
    Nodes = [list_to_atom("antidote@"++StringNode) || StringNode <- StringNodes],
    lists:foreach(fun(Node) ->
                    Res = rpc:call(Node, tx_cert_sup, get_stat, []),
                    io:format("~w", [Res])
                  end, Nodes).
