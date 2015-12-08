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
                non_repl_list,
                num_dcs,
                dc_id,
                populated,
                my_tx_server,
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

    random:seed(now()),
    IPs = basho_bench_config:get(antidote_pb_ips),
    %_PbPorts = basho_bench_config:get(antidote_pb_port),
    MyNode = basho_bench_config:get(antidote_mynode),
    Cookie = basho_bench_config:get(antidote_cookie),

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
    MyTxServer = list_to_atom(atom_to_list(TargetNode) ++ "-cert-" ++ integer_to_list(Id)),
    {PartList, ReplList} =  rpc:call(TargetNode, hash_fun, get_hash_fun, []), %gen_server:call({global, MyTxServer}, {get_hash_fun}),
    FullPartList = lists:flatten([L || {_, L} <- PartList]),
    HashLength = length(FullPartList),
    AllDcs = [N || {N, _} <- PartList],
    NumDcs = length(AllDcs),
    DcId = index(TargetNode, AllDcs),

    lager:info("Part list is ~w",[PartList]),
    ets:new(load, [named_table, public, set]),
    timer:sleep(1000),
    {ok, #state{worker_id=Id,
               my_tx_server=MyTxServer,
               populated=false,
               part_list = PartList,
               repl_list = ReplList,
               full_part_list = FullPartList,
               hash_length = HashLength,   
               num_dcs = NumDcs,
               dc_id = DcId,
               target_node=TargetNode}}.

%% @doc Read a key
run(load, _KeyGen, _ValueGen, State=#state{part_list=PartList, my_tx_server=TxServer, populated=Populated, 
        full_part_list=FullPartList, hash_length=HashLength, num_dcs=NumDCs, dc_id=DcId}) ->
    case Populated of
        false ->
            case DcId of
                1 ->
                    init_params(TxServer, FullPartList, HashLength);
                _ ->
                    ok
            end,
            populate_items(TxServer, NumDCs, DcId, PartList),
            populate_warehouse(TxServer, DcId, PartList),
            populate_stock(TxServer, DcId, PartList),
            populate_district(TxServer, DcId, PartList),
            ets:insert(load, {populated, true}),
            {ok, State#state{populated=true}};
        true ->
            lager:error("Already populated!!"),
            timer:sleep(5000),
            {error, aborted, State}
    end.

populate_items(TxServer, NumDCs, DcId, PartList) ->
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
    Warehouse = tpcc_tool:create_warehouse(DcId),
    WKey = tpcc_tool:get_key(Warehouse),
    WYtd = ?WAREHOUSE_YTD,
    WYtdKey = WKey++":w_ytd",
    lager:info("Populating ~p", [WKey]),
    lager:info("Populating ~p", [WYtdKey]),
    put_to_node(TxServer, DcId, PartList, WYtdKey, WYtd),
    put_to_node(TxServer, DcId, PartList, WKey, Warehouse).

populate_stock(TxServer, WarehouseId, PartList) ->
    Seq = lists:seq(1, ?NB_MAX_ITEM),
    lager:info("Warehouse ~w: Populating stocks from 1 to ~w", [WarehouseId, ?NB_MAX_ITEM]),
    lists:foreach(fun(StockId) ->
                      Stock = tpcc_tool:create_stock(StockId, WarehouseId),
                      Key = tpcc_tool:get_key(Stock),
                      put_to_node(TxServer, WarehouseId, PartList, Key, Stock)
                      end, Seq).

populate_district(TxServer, WarehouseId, PartList) ->
    Seq = lists:seq(1, ?NB_MAX_DISTRICT),
    lager:info("Warehouse ~w: Populating districts from 1 to ~w", [WarehouseId, ?NB_MAX_DISTRICT]),
    lists:foreach(fun(DistrictId) ->
                District = tpcc_tool:create_district(DistrictId, WarehouseId),
                DKey = tpcc_tool:get_key(District),
                put_to_node(TxServer, WarehouseId, PartList, DKey, District),
                DYtd = ?WAREHOUSE_YTD,
                YtdKey = DKey++":d_ytd",
                put_to_node(TxServer, WarehouseId, PartList, YtdKey, DYtd),
                populate_customers(TxServer, WarehouseId, DistrictId, PartList),
                populate_orders(TxServer, WarehouseId, DistrictId, PartList)
                end, Seq).

populate_customers(TxServer, WarehouseId, DistrictId, PartList) ->
    lager:info("Warehouse ~w, district ~w: Populating customers from 1 to ~w", [WarehouseId, DistrictId, 
                ?NB_MAX_CUSTOMER]),
    Seq = lists:seq(1, ?NB_MAX_CUSTOMER),
    lists:foreach(fun(CustomerId) ->
                    CLast = tpcc_tool:c_last(),
                    Customer = tpcc_tool:create_customer(WarehouseId, DistrictId, CustomerId, CLast),
                    CKey = tpcc_tool:get_key(Customer),
                    put_to_node(TxServer, WarehouseId, PartList, CKey, Customer),
                    CBalanceKey = CKey++":c_balance",
                    put_to_node(TxServer, WarehouseId, PartList, CBalanceKey, -10),
                    %CustomerLookup = tpcc_tool:create_customer_lookup(WarehouseId, DistrictId, CLast),
                    TxId = gen_server:call({global, TxServer}, {start_tx}),
                    CLKey = tpcc_tool:get_key_by_param({WarehouseId, DistrictId, CLast}, customer_lookup),
                    CustomerLookup = 
                                case read_from_node(TxServer, TxId, CLKey, WarehouseId, PartList) of
                                    error ->
                                        lager:info("Loading for the first time"),
                                        tpcc_tool:create_customer_lookup(WarehouseId, DistrictId, CLast);
                                    CL ->
                                        CL
                                end,  
                    Ids = CustomerLookup#customer_lookup.ids,
                    lager:info("Adding ~w to ids ~w", [CustomerId, Ids]),
                    CustomerLookup1 = CustomerLookup#customer_lookup{ids=[CustomerId|Ids]}, 
                    put_to_node(TxServer, WarehouseId, PartList, CLKey, CustomerLookup1),
                    History = tpcc_tool:create_history(WarehouseId, DistrictId, CustomerId),
                    HKey = tpcc_tool:get_key(History),
                    put_to_node(TxServer, WarehouseId, PartList, HKey, History)
                end, Seq).

populate_orders(TxServer, WarehouseId, DistrictId, PartList) ->
    lager:info("Warehouse ~w, district ~w: Populating orders from 1 to ~w", [WarehouseId, DistrictId, 
                ?NB_MAX_ORDER]),
    Seq = lists:seq(1, ?NB_MAX_ORDER),
    L = lists:seq(1, ?NB_MAX_CUSTOMER),
    NL = [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- L])],
    lists:foreach(fun(OrderId) ->
                    Date = tpcc_tool:now_nsec(),
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
                    {Amount, DDate} = case OrderId >= ?LIMIT_ORDER of
                                        true ->
                                            {tpcc_order:random_float(0.01, 9999.99, 2), 0};
                                        false ->
                                            {-1, Date}
                                      end,
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
    C_C_LAST = tpcc_tool:random_num(0, ?A_C_LAST),
    C_C_ID = tpcc_tool:random_num(0, ?A_C_ID),
    C_OL_I_ID = tpcc_tool:random_num(0, ?A_OL_I_ID),
    Partition1 = get_partition(K1, FullPartList, HashLength),
    Partition2 = get_partition(K2, FullPartList, HashLength),
    Partition3 = get_partition(K3, FullPartList, HashLength),
    single_put(TxServer, Partition1, K1, C_C_LAST),
    single_put(TxServer, Partition2, K2, C_C_ID),
    single_put(TxServer, Partition3, K3, C_OL_I_ID).

get_partition(Key, PartList, HashLength) ->
    Num = crypto:bytes_to_integer(erlang:md5(Key)) rem HashLength +1,
    lists:nth(Num, PartList).
    
put_to_node(TxServer, DcId, PartList, Key, Value) ->
    {_, L} = lists:nth(DcId, PartList),
    Index = crypto:bytes_to_integer(erlang:md5(Key)) rem length(L) + 1,
    Part = lists:nth(Index, L),    
    single_put(TxServer, Part, Key, Value).

single_put(TxServer, Part, Key, Value) ->
    %lager:info("Puting [~p, ~p] to ~w", [Key, Value, Part]),
    {ok, _} = tx_cert_sup:single_commit(TxServer, Part, Key, Value).

read_from_node(TxServer, TxId, Key, DcId, PartList) ->
    {_, L} = lists:nth(DcId, PartList),
    Index = crypto:bytes_to_integer(erlang:md5(Key)) rem length(L) + 1,
    Part = lists:nth(Index, L),
    {ok, V} = tx_cert_sup:read(TxServer, TxId, Key, Part),
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
