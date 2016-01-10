-module(getAbortStat).

-export([get_stat/1]).

get_stat(StringNodes) ->
    Cookie = antidote,
    true = erlang:set_cookie(node(), Cookie),
    Nodes = [list_to_atom("dev1@"++StringNode) || StringNode <- StringNodes],
    lists:foreach(fun(Node) ->
                    Res = rpc:call(Node, helper, gather_abort_stat, [8]),
                    io:format("~p", [Res])
                  end, Nodes).
