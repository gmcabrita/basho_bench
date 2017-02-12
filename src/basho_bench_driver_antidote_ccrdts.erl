-module(basho_bench_driver_antidote_ccrdts).

-export([new/1,
         run/4]).

-include("../include/basho_bench.hrl").

-define (TIMEOUT, 5000).

-record(state,
  {
    pid,
    num_keys,
    num_players,
    target,
    added_ccrdt,
    added_crdt
 }).

%% ====================================================================
%% API
%% ====================================================================

new(Id) ->

    %% read relevant configuration from config file
    Nodes = basho_bench_config:get(antidote_nodes,['antidote@127.0.0.1']),
    Cookie = basho_bench_config:get(antidote_cookie,antidote),
    NumKeys = basho_bench_config:get(num_keys, 250),
    NumPlayers = basho_bench_config:get(num_players, 500000),

    Target = lists:nth((Id rem length(Nodes)+1), IPs),

    %% Initialize cookie for each of the nodes
    true = erlang:set_cookie(node(), Cookie),
    true = erlang:set_cookie(Target, Cookie),

    %% Seed random number
    rand:seed(exsplus, {erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()}),

    {ok,
      #state {
        pid = Id,
        num_keys = NumKeys,
        num_players = NumPlayers,
        target = Target,
        added_ccrdt = #{},
        added_crdt = #{}
      }
    }.

%%% Top-K with support for deletes

run(topkd_add, _KeyGen, _Value_Gen, State=#state{pid = Id, target = Target, num_keys = NumKeys, num_players = NumPlayers, added_ccrdt = Map}) ->
    Key = rand:uniform(NumKeys),
    PlayerId = rand:uniform(NumPlayers),
    Score = rand:uniform(1000000000),
    Object = {Key, antidote_ccrdt_topk_with_deletes, topk_with_dels},
    Updates = [{Object, add, {PlayerId, Score}}],
    Response = rpc:call(Target, antidote, update_objects, [ignore, [], Updates]),
    case Response of
        {ok, _} ->
            Map1 = case maps:is_key(Key, Map) of
                true ->
                    Tmp = maps:get(Key, Map),
                    New = gb_sets:add(PlayerId, Tmp),
                    maps:put(Key, New, Map);
                false -> maps:put(Key, gb_sets:singleton(PlayerId), Map)
            end,
            {ok, State#state{added_ccrdt = Map1}};
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
    end;

run(topkd_del, _KeyGen, _Value_Gen, State=#state{pid = Id, target = Target, added_ccrdt = Map}) ->
    Key = case maps:keys(Map) of
        [] -> nil;
        ListKeys -> random_element(ListKeys)
    end,
    PlayerId = case maps:is_key(Key, Map) of
        true ->
            case gb_sets:to_list(maps:get(Key, Map)) of
                [] -> nil;
                ListPlayers -> random_element(ListPlayers)
            end;
        false -> nil
    end,
    case PlayerId of
        nil -> {ok, State};
        _ ->
            Object = {Key, antidote_ccrdt_topk_with_deletes, topk_with_dels},
            Updates = [{Object, del, PlayerId}],
            Response = rpc:call(Target, antidote, update_objects, [ignore, [], Updates]),
            case Response of
                {ok, _} ->
                    Set = maps:get(Key, Map),
                    Map1 = maps:put(Key, gb_sets:delete(PlayerId, Set), Map),
                    {ok, State#state{added_ccrdt = Map1}};
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
            end
    end;

%%%% OR-Set modeling a Top-K with removals

run(or_set_topkd_add, _KeyGen, _Value_Gen, State=#state{pid = Id, target = Target, num_keys = NumKeys, num_players = NumPlayers, added_crdt = Map}) ->
    Key = rand:uniform(NumKeys),
    PlayerId = rand:uniform(NumPlayers),
    Score = rand:uniform(1000000000),
    Object = {Key, antidote_crdt_orset, topk_or_set},
    Updates = [{Object, add, {PlayerId, Score}}],
    Response = rpc:call(Target, antidote, update_objects, [ignore, [], Updates]),
    case Response of
        {ok, _} ->
            Map1 = case maps:is_key(Key, Map) of
                true ->
                    PlayerMap = maps:get(Key, Map),
                    NewPlayerMap = case maps:is_key(PlayerId, PlayerMap) of
                        true ->
                            PlayerIdScores = maps:get(PlayerId, PlayerMap),
                            maps:put(PlayerId, [{PlayerId, Score} | PlayerIdScores], PlayerMap);
                        false -> maps:put(PlayerId, [{PlayerId, Score}], PlayerMap)
                    end,
                    maps:put(Key, NewPlayerMap, Map);
                false -> maps:put(Key, maps:from_list([{PlayerId, [{PlayerId, Score}]}]), Map)
            end,
            {ok, State#state{added_crdt = Map1}};
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
    end;

run(or_set_topkd_del, _KeyGen, _Value_Gen, State=#state{pid = Id, target = Target, added_crdt = Map}) ->
    Key = case maps:keys(Map) of
        [] -> nil;
        ListKeys -> random_element(ListKeys)
    end,
    PlayerId = case maps:is_key(Key, Map) of
        true ->
            Tmp = maps:get(Key, Map),
            case maps:keys(Tmp) of
                [] -> nil;
                PlayerIds -> random_element(PlayerIds)
            end;
        false -> nil
    end,
    case PlayerId of
        nil -> {ok, State};
        _ ->
            PlayerMap = maps:get(Key, Map),
            ElementsToRemove = maps:get(PlayerId, PlayerMap),
            Object = {Key, antidote_crdt_orset, topk_or_set},
            Updates = [{Object, remove_all, ElementsToRemove}],
            Response = rpc:call(Target, antidote, update_objects, [ignore, [], Updates]),
            case Response of
                {ok, _} ->
                    NewPlayerMap = maps:remove(PlayerId, PlayerMap),
                    Map1 = maps:put(Key, NewPlayerMap, Map),
                    {ok, State#state{added_crdt = Map1}};
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
            end
    end;

%%%% Plain Top-K

run(topk_add, _KeyGen, _Value_Gen, State=#state{pid = Id, target = Target, num_keys = NumKeys, num_players = NumPlayers}) ->
    Key = rand:uniform(NumKeys),
    PlayerId = rand:uniform(NumPlayers),
    Score = rand:uniform(1000000000),
    Object = {Key, antidote_ccrdt_topk, topk},
    Updates = [{Object, add, {PlayerId, Score}}],
    Response = rpc:call(Target, antidote, update_objects, [ignore, [], Updates]),
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
    end;

%%%% OR-Set modeling a Top-K

run(or_set_topk_add, _KeyGen, _Value_Gen, State=#state{pid = Id, target = Target, num_keys = NumKeys, num_players = NumPlayers}) ->
    Key = rand:uniform(NumKeys),
    PlayerId = rand:uniform(NumPlayers),
    Score = rand:uniform(1000000000),
    Object = {Key, antidote_crdt_orset, plain_topk_orset},
    % playerid and score are inverted here so we can get free ordering in gb_sets
    Element = {Score, PlayerId},
    ResponseRead = rpc:call(Target, antidote, read_objects, [ignore, [], [Object]]),
    case ResponseRead of
        {ok, [ORSet], _} ->
            Set = gb_sets:from_list(ORSet),
            MaxK = max_k(gb_sets:add(Element, Set)),
            case gb_sets:is_member(Element, MaxK) of
                true ->
                    Remove = gb_sets:to_list(gb_sets:difference(Set, MaxK)),
                    Updates = [{Object, add, Element}, {Object, remove_all, Remove}],
                    Response = rpc:call(Target, antidote, update_objects, [ignore, [], Updates]),
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
                    end;
                false -> {ok, State}
            end;
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

max_k(Set) ->
    List = lists:reverse(gb_sets:to_list(Set)),
    {Top, _, _} = reduce_while(
        List,
        {gb_sets:new(), gb_sets:new(), 100},
        fun(_, {_, _, K}) -> K > 0 end,
        fun({_, Id} = E, {T, C, K} = Acc) ->
            case gb_sets:is_member(Id, C) of
                true -> Acc;
                false -> {gb_sets:add(E, T), gb_sets:add(Id, C), K - 1}
            end
        end),
    Top.

reduce_while(Col, Initial, While_Func, Reduce_Func) ->
    try
        lists:foldl(fun (X, Acc) ->
            case While_Func(X, Acc) of
                true -> Reduce_Func(X, Acc);
                false -> throw({halt, Acc})
            end
        end, Initial, Col)
    catch
        throw:{halt, Acc} -> Acc
    end.

random_element(List) ->
    {_, Element} = hd(lists:sort([{rand:uniform(), N} || N <- List])),
    Element.
