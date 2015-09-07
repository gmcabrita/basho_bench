-module(getStat).

-export([get_stat/0]).

get_stat() -> 
    Cookie = antidote,
    true = erlang:set_cookie(node(), Cookie),
    Node = 'antidote@172.31.0.173',
    Info = rpc:call(Node, clocksi_vnode, print_stat,[]),
    io:format("Info is ~w", [Info]).

