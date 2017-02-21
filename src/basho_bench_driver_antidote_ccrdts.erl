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
    topkd_used_keys,
    topkd_orset_used_keys
 }).

%% ====================================================================
%% API
%% ====================================================================

new(Id) ->

    %% read relevant configuration from config file
    Nodes = basho_bench_config:get(antidote_nodes,['antidote@127.0.0.1']),
    Cookie = basho_bench_config:get(antidote_cookie,antidote),
    NumKeys = basho_bench_config:get(num_keys, 1000),
    NumPlayers = basho_bench_config:get(num_players, 50000),

    % Sticky "sessions"
    Target = lists:nth((Id rem length(Nodes)+1), Nodes),

    %% Initialize cookie for each of the nodes
    lager:info("~p~n", [node()]),
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
        topkd_used_keys = sets:new(),
        topkd_orset_used_keys = sets:new()
      }
    }.

%%% Top-K with support for deletes

run(topkd_add, _KeyGen, _Value_Gen, State=#state{pid = Id,
                                                 target = Target,
                                                 num_keys = NumKeys,
                                                 num_players = NumPlayers,
                                                 topkd_used_keys = UsedKeys}) ->
    Key = rand:uniform(NumKeys),
    PlayerId = rand:uniform(NumPlayers),
    Score = rand:uniform(250000),
    Object = {Key, antidote_ccrdt_topk_rmv, topkd},
    Updates = [{Object, add, {PlayerId, Score}}],
    Response = rpc:call(Target, antidote, update_objects, [ignore, [], Updates]),
    case Response of
        {ok, _} ->
            {ok, State#state{topkd_used_keys = sets:add_element(Key, UsedKeys)}};
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

run(topkd_del, _KeyGen, _Value_Gen, State=#state{pid = Id,
                                                 target = Target,
                                                 topkd_used_keys = UsedKeys}) ->
    case sets:size(UsedKeys) of
        0 -> {ok, State};
        _ ->
            Key = random_element(sets:to_list(UsedKeys)),
            Object = {Key, antidote_ccrdt_topk_rmv, topkd},
            ResponseRead = rpc:call(Target, antidote, read_objects, [ignore, [], [Object]]),
            case ResponseRead of
                {ok, [[]], _} ->
                    %% in this case the top-K was empty
                    {ok, State#state{topkd_used_keys = sets:del_element(Key, UsedKeys)}};
                {ok, [TopK], _} ->
                    {PlayerId, _} = random_element(TopK),
                    Updates = [{Object, rmv, PlayerId}],
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

run(or_set_topkd_add, _KeyGen, _Value_Gen, State=#state{pid = Id,
                                                        target = Target,
                                                        num_keys = NumKeys,
                                                        num_players = NumPlayers,
                                                        topkd_orset_used_keys = UsedKeys}) ->
    Key = rand:uniform(NumKeys),
    PlayerId = rand:uniform(NumPlayers),
    Score = rand:uniform(250000),
    Element = {PlayerId, Score},
    Object = {Key, antidote_crdt_orset, topkd_orset},
    Updates = [{Object, add, Element}],
    Response = rpc:call(Target, antidote, update_objects, [ignore, [], Updates]),
    case Response of
        {ok, _} ->
            {ok, State#state{topkd_orset_used_keys = sets:add_element(Key, UsedKeys)}};
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

run(or_set_topkd_del, _KeyGen, _Value_Gen, State=#state{pid = Id,
                                                        target = Target,
                                                        topkd_orset_used_keys = UsedKeys}) ->
    case sets:size(UsedKeys) of
        0 -> {ok, State};
        _ ->
            Key = random_element(sets:to_list(UsedKeys)),
            Object = {Key, antidote_crdt_orset, topkd_orset},
            ResponseRead = rpc:call(Target, antidote, read_objects, [ignore, [], [Object]]),
            case ResponseRead of
                {ok, [[]], _} ->
                    %% in this case the top-K was empty
                    {ok, State#state{topkd_orset_used_keys = sets:del_element(Key, UsedKeys)}};
                {ok, [TopK], _} ->
                    {PlayerId, _} = random_element(TopK),
                    ElementsToRemove = lists:filter(fun({I, _}) ->
                        I == PlayerId
                    end, TopK),
                    Updates = [{Object, remove_all, ElementsToRemove}],
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

run(topk_add, _KeyGen, _Value_Gen, State=#state{pid = Id,
                                                target = Target,
                                                num_keys = NumKeys,
                                                num_players = NumPlayers}) ->
    Key = rand:uniform(NumKeys),
    PlayerId = rand:uniform(NumPlayers),
    Score = rand:uniform(250000),
    Element = {PlayerId, Score},
    Object = {Key, antidote_ccrdt_topk, topk},
    ResponseRead = rpc:call(Target, antidote, read_objects, [ignore, [], [Object]]),
    case ResponseRead of
        {ok, [TopK], _} ->
            ShouldRequest = lists:foldl(fun({I, S}, Acc) ->
                case I == PlayerId of
                    true -> Score > S;
                    false -> Score > S andalso Acc
                end
            end, false, TopK),
            case ShouldRequest of
                true ->
                    Updates = [{Object, add, Element}],
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
    end;

%%%% OR-Set modeling a Top-K

run(or_set_topk_add, _KeyGen, _Value_Gen, State=#state{pid = Id,
                                                       target = Target,
                                                       num_keys = NumKeys,
                                                       num_players = NumPlayers}) ->
    Key = rand:uniform(NumKeys),
    PlayerId = rand:uniform(NumPlayers),
    Score = rand:uniform(250000),
    Object = {Key, antidote_crdt_orset, topk_orset},
    Element = {PlayerId, Score},
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
                false ->
                    case gb_sets:to_list(gb_sets:difference(Set, MaxK)) of
                        [] -> {ok, State};
                        Remove ->
                            Updates = [{Object, remove_all, Remove}],
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
                            end
                    end
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
    List = gb_sets:to_list(Set),
    SortedList = lists:sort(fun(X,Y) -> cmp(X, Y) end, List),
    {Top, _, _} = reduce_while(
        SortedList,
        {gb_sets:new(), gb_sets:new(), 100},
        fun(_, {_, _, K}) -> K > 0 end,
        fun({_, Id} = E, {T, C, K} = Acc) ->
            case gb_sets:is_member(Id, C) of
                true -> Acc;
                false -> {gb_sets:add(E, T), gb_sets:add(Id, C), K - 1}
            end
        end),
    Top.

cmp({Id1, Score1}, {Id2, Score2}) ->
    Score1 > Score2 orelse (Score1 == Score2 andalso Id1 > Id2).

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
