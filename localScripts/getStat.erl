-module(getStat).

-export([get_stat/0]).

get_stat() ->
    Cookie = antidote,
    true = erlang:set_cookie(node(), Cookie),
    Nodes = ['dev1@127.0.0.1', 'dev2@127.0.0.1', 'dev3@127.0.0.1'],
    L=[1,2,3,4,5],
    {R1, R2} = lists:foldl(fun(Node, {Acc1, Acc2}) ->
                        lists:foldl(fun(N, {A1, C1}) -> 
                            {T1, T2} = rpc:call(Node, tx_cert_sup, get_stat, [N]),
                            {A1+T1, C1+T2} end, {Acc1, Acc2}, L)
                     end, {0,0}, Nodes),
    io:format("~w, ~w", [R1, R2]).
