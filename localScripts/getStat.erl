-module(getStat).

-export([get_stat/1]).

get_stat(StringNodes) ->
    Cookie = antidote,
    true = erlang:set_cookie(node(), Cookie),
    Nodes = [list_to_atom("antidote@"++StringNode) || StringNode <- StringNodes],
    io:format("Nodes are ~w", [Nodes]),
    {R1, R2, R3, R4, R5, R6, R7, R8, R9, R10} = lists:foldl(fun(Node, {A1, A2, A3, A4, A5, A6, A7, A8, A9, A10}) ->
                            {T1, T2, T3, T4, T5, T6, T7, T8, T9, T10} = rpc:call(Node, tx_cert_sup, get_stat, []),
                            {A1+T1, A2+T2, A3+T3, A4+T4, A5+T5, A6+T6, A7+T7, A8+T8, A9+T9, A10+T10}  
                     end, {0,0,0,0,0,0,0,0,0,0}, Nodes),
    io:format("~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w", [R1, R2, R3, R4, R5, R6, R7, R8, R9, R10]).
