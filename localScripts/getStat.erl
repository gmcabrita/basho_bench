-module(getStat).

-export([get_stat/0]).

get_stat() ->
    Cookie = antidote,
    true = erlang:set_cookie(node(), Cookie),
    Nodes = ['dev1@127.0.0.1', 'dev2@127.0.0.1', 'dev3@127.0.0.1'],
    L=[1,2,3,4,5],
    {R1, R2, R3, R4, R5} = lists:foldl(fun(Node, {Acc1, Acc2, Acc3, Acc4, Acc5}) ->
                        lists:foldl(fun(N, {A1, A2, A3, A4, A5}) -> 
                            {T1, T2, T3, T4, T5} = rpc:call(Node, tx_cert_sup, get_stat, [N]),
                            {A1+T1, A2+T2, A3+T3, A4+T4, A5+T5} end, {Acc1, Acc2, Acc3, Acc4, Acc5}, L)
                     end, {0,0,0,0,0}, Nodes),
    io:format("~w, ~w, ~w, ~w, ~w", [R1, R2, R3, R4, R5]).
