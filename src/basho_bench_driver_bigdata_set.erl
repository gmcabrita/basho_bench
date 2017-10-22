-module(basho_bench_driver_bigdata_set).

-export([new/1,
         run/4]).

-include("../include/basho_bench.hrl").

-define (TIMEOUT, 5000).

-record(state,
  {
    pid,
    file_pid,
    target
 }).

%% ====================================================================
%% API
%% ====================================================================

new(Id) ->

    %% read relevant configuration from config file
    Nodes = basho_bench_config:get(antidote_nodes,['antidote@127.0.0.1']),
    Cookie = basho_bench_config:get(antidote_cookie,antidote),

    [Target | _] = Nodes,

    %% Initialize cookie for each of the nodes
    lager:info("~p~n", [node()]),
    true = erlang:set_cookie(node(), Cookie),
    true = erlang:set_cookie(Target, Cookie),

    Location = basho_bench_config:get(bigdata,"/home/jcalbuquerque/basho_bench/") ++ integer_to_list(Id),
    {ok, FilePid} = file:open(Location, [read, binary]),

    %% Seed random number
    rand:seed(exsplus, {erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()}),

    {ok,
      #state {
        pid = Id,
        file_pid = FilePid,
        target = Target
      }
    }.

run(add, _KeyGen, _Value_Gen, State=#state{pid = Id, file_pid = FPid, target = Target}) ->
    {ok, Data} = file:read_line(FPid),
    {TweetId, Tweet, Followers} = get_data(Data),
    Split = split(Tweet),
    Object = {a, antidote_crdt_gmap, bucket},
    Updates = maps:fold(fun(K, V, Acc) ->
        [{{K, antidote_crdt_gset}, {add, {TweetId, Followers*V}}} | Acc]
    end, [], Split),
    Response = rpc:call(Target, antidote, update_objects, [ignore, [], [{Object, {update, Updates}}]]),
    case Response of
        {ok, _} ->
            {ok, State};
        {error,timeout} ->
            lager:info("Timeout on client ~p",[Id]),
            {error, timeout, State};
        {error, Reason} ->
            lager:error("Error: ~p",[Reason]),
            {error, Reason, State};
        error ->
            {error, abort, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end.

get_data(Data) ->
    M = jsx:decode(Data, [return_maps]),
    {maps:get(<<"id">>, M), maps:get(<<"text">>, M), maps:get(<<"count">>, M)}.

split(Text) ->
    Split = binary:split(Text, [<<"\n">>, <<" ">>], [global]),
    lists:foldl(fun(Word, Acc) ->
        case maps:is_key(Word, Acc) of
            true ->
                V = maps:get(Word, Acc),
                maps:update(Word, V + 1, Acc);
            false ->
                maps:put(Word, 1, Acc)
        end
    end, #{}, Split).

