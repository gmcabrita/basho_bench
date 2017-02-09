-module(basho_bench_driver_antidote_trigger_bug).

-export([new/1,
         run/4]).

-include("../include/basho_bench.hrl").

-define (TIMEOUT, 5000).

-record(state,
  {
    pid,
    nodes
 }).

%% ====================================================================
%% API
%% ====================================================================

new(Id) ->

    %% read relevant configuration from config file
    Nodes = basho_bench_config:get(antidote_nodes,['antidote@127.0.0.1']),
    Cookie = basho_bench_config:get(antidote_cookie,antidote),

    %% Initialize cookie for each of the nodes
    true = erlang:set_cookie(node(), Cookie),
    lists:foreach(fun(N) -> erlang:set_cookie(N, Cookie) end, Nodes),

    %% Seed random number
    rand:seed(exsplus, {erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()}),

    {ok,
      #state {
        pid = Id,
        nodes = Nodes
      }
    }.

run(trigger_bug, _KeyGen, _Value_Gen, State=#state{pid = Id, nodes = Nodes}) ->
    Target = random_element(Nodes),
    Value  = rand:uniform(1000),
    Object = {1, antidote_crdt_orset, some_bucket},
    Updates = [{Object, add, Value}],
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
    end.

random_element(List) ->
    {_, Element} = hd(lists:sort([{rand:uniform(), N} || N <- List])),
    Element.
