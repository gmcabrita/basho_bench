-module(getStat).

-export([get_vnode_stat/0, get_time_stat/0]).

get_vnode_stat() -> 
    Cookie = antidote,
    true = erlang:set_cookie(node(), Cookie),
    Node = 'antidote@172.31.0.173',
    Info = rpc:call(Node, clocksi_vnode, print_stat,[]),
    io:format("Info is ~w", [Info]).

get_time_stat() -> 
    Nodes = ['antidote@172.31.0.106', 'antidote@172.31.0.117', 'antidote@172.31.0.173', 'antidote@172.31.0.5'],
    Cookie = antidote,
    true = erlang:set_cookie(node(), Cookie),
    AllInfo = lists:foldl(fun(Node, Acc) -> Info= rpc:call(Node, stat_server, get_stat, [Node]),
                        [Info|Acc] end, [], Nodes),
    io:format("Info is ~w", [AllInfo]).
