%% -------------------------------------------------------------------
%%
%% basho_bench: Benchmarking Suite
%%
%% Copyright (c) 2009-2010 Basho Techonologies
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(basho_bench_driver_load).

-export([new/1,
         run/4]).

-include("basho_bench.hrl").
-include("tpcc.hrl").

-define(TIMEOUT, 20000).

-record(state, {worker_id,
                part_list,
                full_part_list,
                hash_length,
                repl_list,
                commit_time,
                non_repl_list,
                stage,
                num_dcs,
                dc_id,
                my_tx_server,
                replicas,
                district_id,
                target_node}).

%% ====================================================================
%% API
%% ====================================================================

new(Id) ->
    %% Make sure bitcask is available
    case code:which(antidote) of
        non_existing ->
            ?FAIL_MSG("~s requires antidote to be available on code path.\n",
                      [?MODULE]);
        _ ->
            ok
    end,

    random:seed(now()), %now()),
    IPs = basho_bench_config:get(antidote_pb_ips),
    %_PbPorts = basho_bench_config:get(antidote_pb_port),
    MyNode = basho_bench_config:get(antidote_mynode),
    Cookie = basho_bench_config:get(antidote_cookie),
    ToSleep = basho_bench_config:get(to_sleep),

    case net_kernel:start(MyNode) of
        {ok, _} ->
            ?INFO("Net kernel started as ~p\n", [node()]);
        {error, {already_started, _}} ->
            ?INFO("Net kernel already started as ~p\n", [node()]),
            ok;
        {error, Reason} ->
            ?FAIL_MSG("Failed to start net_kernel for ~p: ~p\n", [?MODULE, Reason])
    end,

    %% Choose the node using our ID as a modulus
    TargetNode = lists:nth((Id rem length(IPs)+1), IPs),
    true = erlang:set_cookie(node(), Cookie),

    ?INFO("Using target node ~p for worker ~p\n", [TargetNode, Id]),
    Result = net_adm:ping(TargetNode),
    ?INFO("Result of ping is ~p \n", [Result]),
    MyTxServer = {server, list_to_atom(atom_to_list(TargetNode) ++ "-cert-" ++ integer_to_list(Id))},
    {PartList, ReplList} =  rpc:call(TargetNode, hash_fun, get_hash_fun, []), %gen_server:call({global, MyTxServer}, {get_hash_fun}),
    FullPartList = lists:flatten([L || {_, L} <- PartList]),
    HashLength = length(FullPartList),
    AllDcs = [N || {N, _} <- PartList],
    [M] = [L || {N, L} <- ReplList, N == TargetNode ],
    MyRepIds = get_indexes(M, AllDcs),
    MyReps = lists:map(fun(Index) ->  Name = lists:nth(Index, AllDcs),  {Index, {rep, get_rep_name(TargetNode, Name)}} end, MyRepIds),
    NumDcs = length(AllDcs),
    DcId = index(TargetNode, AllDcs),

    lager:info("TargetNode is ~p, DcId is ~w, My Replica Ids are ~w",[TargetNode, DcId, MyReps]),
    ets:new(list_to_atom(integer_to_list(DcId)), [named_table, public, set]),
    timer:sleep(ToSleep),
    {ok, #state{worker_id=Id,
               my_tx_server=MyTxServer,
               replicas=MyReps,
               part_list = PartList,
               repl_list = ReplList,
               full_part_list = FullPartList,
               hash_length = HashLength,   
               num_dcs = NumDcs,
               dc_id = DcId,
               stage=init,
               target_node=TargetNode}}.

%% @doc Read a key
run(load, _KeyGen, _ValueGen, State=#state{part_list=PartList, my_tx_server=TxServer, district_id=DistrictId, replicas=RepIds,
        stage=Stage, full_part_list=FullPartList, hash_length=HashLength, num_dcs=NumDCs, dc_id=DcId}) ->
    case Stage of
        init ->
           case DcId of
                1 ->
                    init_params(TxServer, FullPartList, HashLength);
                _ ->
                    ok
           end,
       %% Sleep to make sure that COMMIT_TIME is written to a partition
       timer:sleep(3000),
       COMMIT_TIME = read(TxServer, "COMMIT_TIME", FullPartList, HashLength),
       ets:insert(list_to_atom(integer_to_list(DcId)), {"COMMIT_TIME", COMMIT_TIME}),
       lager:info("CommitTime is ~w", [COMMIT_TIME]),
       {ok, State#state{stage=to_item}};
    to_item -> 
        lager:info("Populating items"),
        populate_items(TxServer, NumDCs, DcId, PartList),
        lists:foreach(fun({RepDcId, Replica}) -> populate_items(Replica, NumDCs, RepDcId, PartList) end, 
                  RepIds),
        {ok, State#state{stage=to_warehouse}};
    to_warehouse -> 
        lager:info("Populating warehouse"),
        populate_warehouse(TxServer, DcId, PartList),
        lists:foreach(fun({RepDcId, Replica}) -> populate_warehouse(Replica, RepDcId, PartList) end, 
                  RepIds),
        {ok, State#state{stage=to_stock}};
    to_stock ->
        lager:info("Populating stocks"),
        populate_stock(TxServer, DcId, PartList),
        lists:foreach(fun({RepDcId, Replica}) -> populate_stock(Replica, RepDcId, PartList) end, 
                 RepIds),
        {ok, State#state{stage=to_district, district_id=1}};
    to_district ->
        case DistrictId =< ?NB_MAX_DISTRICT of
            true ->
                lager:info("Populating districts ~w", [DistrictId]),
                populate_district(TxServer, DcId, DistrictId, PartList),
                lists:foreach(fun({RepDcId, Replica}) -> populate_district(Replica, RepDcId, DistrictId, PartList) end, 
                    RepIds),
                {ok, State#state{district_id=DistrictId+1}};
            false ->
                {ok, State#state{stage=to_customer, district_id=1}}
        end;
    to_customer ->
        case DistrictId =< ?NB_MAX_DISTRICT of
            true ->
                lager:info("Populating customers for ~w", [DistrictId]),
                populate_customers(TxServer, DcId, DistrictId, PartList),
                lists:foreach(fun({RepDcId, Replica}) -> populate_customers(Replica, RepDcId, DistrictId, PartList) end, 
                    RepIds),
                {ok, State#state{district_id=DistrictId+1}};
            false ->
                {ok, State#state{stage=to_order, district_id=1}}
        end;
    to_order ->
        case DistrictId =< ?NB_MAX_DISTRICT of
            true ->
                lager:info("Populating orders for ~w", [DistrictId]),
                populate_orders(TxServer, DcId, DistrictId, PartList),
                lists:foreach(fun({RepDcId, Replica}) -> populate_orders(Replica, RepDcId, DistrictId, PartList) end, 
                    RepIds),
                {ok, State#state{district_id=DistrictId+1}};
            false ->
                {ok, State#state{stage=finished}}
        end;
    finished ->
        lager:error("Already populated!!"),
        timer:sleep(5000),
        {error, aborted, State}
    end.

populate_items(TxServer, NumDCs, DcId, PartList) ->
    random:seed({DcId, 122,32}),
    Remainder = ?NB_MAX_ITEM rem NumDCs,
    DivItems = (?NB_MAX_ITEM-Remainder)/NumDCs,
    FirstItem = ((DcId-1) * DivItems) + 1,
    LastItem = case DcId of
                    NumDCs ->
                        DivItems + Remainder + FirstItem - 1;
                    _ ->
                        DivItems + FirstItem -1
                   end,
    put_range_items(TxServer, trunc(FirstItem), trunc(LastItem), DcId, PartList).

populate_warehouse(TxServer, DcId, PartList)->
    random:seed({DcId, 12,32}),
    Warehouse = tpcc_tool:create_warehouse(DcId),
    WKey = tpcc_tool:get_key(Warehouse),
    WYtd = ?WAREHOUSE_YTD,
    WYtdKey = WKey++":w_ytd",
    lager:info("Populating ~p", [WKey]),
    lager:info("Populating ~p", [WYtdKey]),
    put_to_node(TxServer, DcId, PartList, WYtdKey, WYtd),
    put_to_node(TxServer, DcId, PartList, WKey, Warehouse).

populate_stock(TxServer, WarehouseId, PartList) ->
    random:seed({WarehouseId, 2,222}),
    Seq = lists:seq(1, ?NB_MAX_ITEM),
    lager:info("Warehouse ~w: Populating stocks from 1 to ~w", [WarehouseId, ?NB_MAX_ITEM]),
    lists:foreach(fun(StockId) ->
                      Stock = tpcc_tool:create_stock(StockId, WarehouseId),
                      Key = tpcc_tool:get_key(Stock),
                      put_to_node(TxServer, WarehouseId, PartList, Key, Stock)
                      end, Seq).

populate_district(TxServer, WarehouseId, DistrictId, PartList) ->
    random:seed({WarehouseId, DistrictId, 22}),
    lager:info("Warehouse ~w: Populating district ~w", [WarehouseId, DistrictId]),
    District = tpcc_tool:create_district(DistrictId, WarehouseId),
    DKey = tpcc_tool:get_key(District),
    put_to_node(TxServer, WarehouseId, PartList, DKey, District),
    DYtd = ?WAREHOUSE_YTD,
    YtdKey = DKey++":d_ytd",
    put_to_node(TxServer, WarehouseId, PartList, YtdKey, DYtd).

populate_customers(TxServer, WarehouseId, DistrictId, PartList) ->
    lager:info("Warehouse ~w, district ~w: Populating customers from 1 to ~w", [WarehouseId, DistrictId, 
                ?NB_MAX_CUSTOMER]),
    Seq = lists:seq(1, ?NB_MAX_CUSTOMER),
    [{"COMMIT_TIME", COMMIT_TIME}] = ets:lookup(list_to_atom(integer_to_list(WarehouseId)), "COMMIT_TIME"),
    HistoryTime = COMMIT_TIME - 123,
    CustomerTime = COMMIT_TIME - 13,
    lists:foreach(fun(CustomerId) ->
                    random:seed({WarehouseId, DistrictId, CustomerId}),
                    CLast = tpcc_tool:c_last(WarehouseId),
                    Customer = tpcc_tool:create_customer(WarehouseId, DistrictId, CustomerId, CLast, CustomerTime),
                    CKey = tpcc_tool:get_key(Customer),
                    put_to_node(TxServer, WarehouseId, PartList, CKey, Customer),
                    CBalanceKey = CKey++":c_balance",
                    put_to_node(TxServer, WarehouseId, PartList, CBalanceKey, -10),
                    %CustomerLookup = tpcc_tool:create_customer_lookup(WarehouseId, DistrictId, CLast),
                    CLKey = tpcc_tool:get_key_by_param({WarehouseId, DistrictId, CLast}, customer_lookup),
                    CustomerLookup = 
                                case read_from_node(TxServer, CLKey, WarehouseId, PartList) of
                                    error ->
                                        %lager:info("Loading for the first time"),
                                        tpcc_tool:create_customer_lookup(WarehouseId, DistrictId, CLast);
                                    CL ->
                                        CL
                                end,  
                    Ids = CustomerLookup#customer_lookup.ids,
                    %lager:info("Adding ~w to ids ~w", [CustomerId, Ids]),
                    CustomerLookup1 = CustomerLookup#customer_lookup{ids=[CustomerId|Ids]}, 
                    put_to_node(TxServer, WarehouseId, PartList, CLKey, CustomerLookup1),
                    History = tpcc_tool:create_history(WarehouseId, DistrictId, CustomerId, HistoryTime),
                    HKey = tpcc_tool:get_key(History),
                    put_to_node(TxServer, WarehouseId, PartList, HKey, History)
                end, Seq).

populate_orders(TxServer, WarehouseId, DistrictId, PartList) ->
    lager:info("Warehouse ~w, district ~w: Populating orders from 1 to ~w", [WarehouseId, DistrictId, 
                ?NB_MAX_ORDER]),
    Seq = lists:seq(1, ?NB_MAX_ORDER),
    L = lists:seq(1, ?NB_MAX_CUSTOMER),
    random:seed({WarehouseId, DistrictId, 1111}),
    NL = [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- L])],
    [{"COMMIT_TIME", CommitTime}] = ets:lookup(list_to_atom(integer_to_list(WarehouseId)), "COMMIT_TIME"),
    %% Magic number, assume that the order is created 1 sec ago.
    Date = CommitTime - 1000,
    lists:foreach(fun(OrderId) ->
                    %Date = tpcc_tool:now_nsec(),
                    random:seed({WarehouseId, DistrictId, OrderId}),
                    OOlCnt = tpcc_tool:random_num(5, 15),
                    Order = tpcc_tool:create_order(WarehouseId, DistrictId, OrderId, OOlCnt, lists:nth(OrderId, NL),
                        Date),
                    OKey = tpcc_tool:get_key(Order),
                    put_to_node(TxServer, WarehouseId, PartList, OKey, Order),
                    populate_orderlines(TxServer, WarehouseId, DistrictId, OrderId, OOlCnt, Date, PartList),
                    case OrderId >= ?LIMIT_ORDER of
                        true ->
                            NewOrder = tpcc_tool:create_neworder(WarehouseId, DistrictId, OrderId),
                            NOKey = tpcc_tool:get_key(NewOrder),
                            put_to_node(TxServer, WarehouseId, PartList, NOKey, NewOrder);
                        false ->
                            ok
                    end
                end, Seq).

populate_orderlines(TxServer, WarehouseId, DistrictId, OrderId, OOlCnt, Date, PartList) ->
    %lager:info("Warehouse ~w, district ~w: Populating orderlines from 1 to ~w", [WarehouseId, DistrictId, 
    %            OOlCnt]),
    Seq = lists:seq(1, OOlCnt),
    lists:foreach(fun(OrderlineId) -> 
                    random:seed({WarehouseId, DistrictId, OrderlineId}),
                    {Amount, DDate} = case OrderId >= ?LIMIT_ORDER of
                                        true ->
                                            {tpcc_order:random_float(0.01, 9999.99, 2), 0};
                                        false ->
                                            {-1, Date}
                                      end,
                    random:seed({WarehouseId, DistrictId, OrderlineId+10}),
                    OrderLine = tpcc_tool:create_orderline(WarehouseId, DistrictId, OrderId, OrderlineId, DDate, Amount),
                    OLKey = tpcc_tool:get_key(OrderLine),
                    put_to_node(TxServer, WarehouseId, PartList, OLKey, OrderLine)
                end, Seq).
    
put_range_items(TxServer, FirstItem, LastItem, DcId, PartList) ->
    lager:info("Populating items from ~w to ~w", [FirstItem, LastItem]),
    Seq = lists:seq(FirstItem, LastItem),
    lists:foreach(fun(ItemId) ->
                    Item = tpcc_tool:create_item(ItemId),
                    Key = tpcc_tool:get_key(Item),
                    put_to_node(TxServer, DcId, PartList, Key, Item)
                    end, Seq).

init_params(TxServer, FullPartList, HashLength) ->
    K1 = "C_C_LAST",
    K2 = "C_C_ID",
    K3 = "C_OL_I_ID",
    K4 = "COMMIT_TIME", 
    C_C_LAST = tpcc_tool:random_num(0, ?A_C_LAST),
    C_C_ID = tpcc_tool:random_num(0, ?A_C_ID),
    C_OL_I_ID = tpcc_tool:random_num(0, ?A_OL_I_ID),
    COMMIT_TIME = tpcc_tool:now_nsec() - 5000,
    Partition1 = get_partition(K1, FullPartList, HashLength),
    Partition2 = get_partition(K2, FullPartList, HashLength),
    Partition3 = get_partition(K3, FullPartList, HashLength),
    Partition4 = get_partition(K4, FullPartList, HashLength),
    lager:info("Putting CCLAST to ~w", [Partition1]),
    lager:info("Putting CCID to ~w", [Partition2]),
    lager:info("Putting COLIID to ~w", [Partition3]),
    lager:info("Putting COMMIT_TIME to ~w", [Partition4]),
    single_put(TxServer, Partition1, K1, C_C_LAST, COMMIT_TIME),
    single_put(TxServer, Partition2, K2, C_C_ID, COMMIT_TIME),
    single_put(TxServer, Partition3, K3, C_OL_I_ID, COMMIT_TIME),
    single_put(TxServer, Partition4, K4, COMMIT_TIME, COMMIT_TIME).

get_partition(Key, PartList, HashLength) ->
    Num = crypto:bytes_to_integer(erlang:md5(Key)) rem HashLength +1,
    lists:nth(Num, PartList).
    
put_to_node(TxServer, DcId, PartList, Key, Value) ->
    {_, L} = lists:nth(DcId, PartList),
    Index = crypto:bytes_to_integer(erlang:md5(Key)) rem length(L) + 1,
    Part = lists:nth(Index, L),    
    [{"COMMIT_TIME", CommitTime}] = ets:lookup(list_to_atom(integer_to_list(DcId)), "COMMIT_TIME"),
    single_put(TxServer, Part, Key, Value, CommitTime).

single_put(TxServer, Part, Key, Value, CommitTime) ->
    %lager:info("Single Puting [~p, ~p] to ~w", [Key, Value, Part]),
    case TxServer of
        {server, S} ->
            {ok, _} = tx_cert_sup:append_value(S, Part, Key, Value, CommitTime);
        {rep, S} ->
            ok = data_repl_serv:append_value(S, Key, Value, CommitTime)
    end.

read_from_node(TxServer, Key, DcId, PartList) ->
    {_, L} = lists:nth(DcId, PartList),
    Index = crypto:bytes_to_integer(erlang:md5(Key)) rem length(L) + 1,
    Part = lists:nth(Index, L),
    {ok, V} = case TxServer of
                {server, S} ->
                    tx_cert_sup:single_read(S, Key, Part);
                {rep, S} ->
                    data_repl_serv:single_read(S, Key)
            end,
    case V of
        [] ->
            lager:error("Key ~p not found!!!!", [Key]),
            error;
        _ ->
            V
    end.

index(Elem, L) ->
    index(Elem, L, 1).

index(_, [], _) ->
    -1;
index(E, [E|_], N) ->
    N;
index(E, [_|L], N) ->
    index(E, L, N+1).

get_indexes(PL, List) ->
    lager:info("Trying to get index: PL ~w, List ~w", [PL, List]),
    [index(X, List) || X <- PL ].

read({server, TxServer}, Key, ExpandPartList, HashLength) ->
    Part = get_partition(Key, ExpandPartList, HashLength),
    {ok, V} = tx_cert_sup:single_read(TxServer, Key, Part),
    case V of
        [] ->
            lager:error("Key ~p not found!!!!", [Key]),
            error;
        _ ->
            V
    end.

get_rep_name(Target, Rep) ->
    list_to_atom(atom_to_list(Target)++"repl"++atom_to_list(Rep)).

