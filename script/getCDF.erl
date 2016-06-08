-module(getCDF).

-export([get_cdf/1]).

get_cdf(StringNodes) ->
    Cookie = antidote,
    true = erlang:set_cookie(node(), Cookie),
    Nodes = [list_to_atom("antidote@"++StringNode) || StringNode <- StringNodes],
    lists:foreach(fun(Node) ->
                    Res = rpc:call(Node, tx_cert_sup, get_cdf, []),
                    io:format("~w", [Res])
                  end, Nodes).
