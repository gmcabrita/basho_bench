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
		        start_time,
                full_part_list,
                my_pop_range,
                hash_length,
                repl_list,
                commit_time,
                non_repl_list,
                populate_list,
                w_per_dc,
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
    WPerDc = basho_bench_config:get(w_per_dc),
    TotalThreads = basho_bench_config:get(concurrent),

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
    TargetNode = lists:nth(((Id-1) rem length(IPs)+1), IPs),
    ThreadPerDc = TotalThreads div length(IPs),
    true = erlang:set_cookie(node(), Cookie),

    ?INFO("Using target node ~p for worker ~p\n", [TargetNode, Id]),
    Result = net_adm:ping(TargetNode),
    ?INFO("Result of ping is ~p \n", [Result]),
    MyTxServer = {server, list_to_atom(atom_to_list(TargetNode) ++ "-cert-" ++ integer_to_list(Id))},
    {PartList, ReplList} =  rpc:call(TargetNode, hash_fun, get_hash_fun, []), %gen_server:call({global, MyTxServer}, {get_hash_fun}),
    %lager:info("Part list is ~w, Replist is ~w", [PartList, ReplList]),
    FullPartList = lists:flatten([L || {_, L} <- PartList]),
    HashLength = length(FullPartList),
    AllDcs = [N || {N, _} <- PartList],
    [M] = [L || {N, L} <- ReplList, N == TargetNode ],
    MyRepIds = get_indexes(M, AllDcs),
    MyReps = lists:map(fun(Index) ->  Name = lists:nth(Index, AllDcs),  {Index, {rep, get_rep_name(TargetNode, Name)}} end, MyRepIds),
    NumDcs = length(AllDcs),
    DcId = index(TargetNode, AllDcs),
    timer:sleep(ToSleep),

    lager:info("TargetNode is ~p, DcId is ~w, My Replica Ids are ~w",[TargetNode, DcId, MyReps]),
    case Id of 1 -> ets:new(meta_info, [named_table, public, set]);
         _ -> ok
    end,
    NewId =  (Id-1) div length(IPs)+1,
    ToPopulate = length(MyReps)+1,
    ToPopPerThread = ToPopulate div ThreadPerDc, 
    MyRange = case NewId of
                ThreadPerDc ->
                    lists:sublist([{DcId, MyTxServer}|MyReps], ToPopPerThread*(NewId-1)+1, ToPopPerThread + ToPopulate rem ThreadPerDc);
                _ ->
                    lists:sublist([{DcId, MyTxServer}|MyReps], ToPopPerThread*(NewId-1)+1, ToPopPerThread)
    end,
    lager:info("My real Id ~w, new Id ~w, range ~w", [Id, NewId, MyRange]),
    
    StartTime = os:timestamp(),
    {ok, #state{worker_id=NewId,
               my_tx_server=MyTxServer,
               my_pop_range=MyRange,
               start_time=StartTime,
               replicas=MyReps,
               part_list = PartList,
               repl_list = ReplList,
               full_part_list = FullPartList,
               hash_length = HashLength,   
               w_per_dc = WPerDc,
               num_dcs = NumDcs,
               dc_id = DcId,
               stage=init,
               target_node=TargetNode}}.

%% @doc Read a key
run(load, _KeyGen, _ValueGen, State=#state{part_list=PartList, my_tx_server=TxServer, district_id=DistrictId, my_pop_range=PopRange, worker_id=Id,
        stage=Stage, w_per_dc=WPerDc, full_part_list=FullPartList, hash_length=HashLength, num_dcs=NumDCs, dc_id=DcId, start_time=StartedTime}) ->
    case Stage of
        init ->
            case (DcId==1) and (Id==1) of
                true ->
                    init_params(TxServer, FullPartList, HashLength);
                _ ->
                    ok
            end,
            timer:sleep(3000),
            case Id of
                1 ->
       %% Sleep to make sure that COMMIT_TIME is written to a partition
                    COMMIT_TIME = read(TxServer, "COMMIT_TIME", FullPartList, HashLength),
                    ets:insert(meta_info, {"COMMIT_TIME", COMMIT_TIME}),
                    lager:info("CommitTime is ~w", [COMMIT_TIME]);
                _ ->
                    ok
            end,
            timer:sleep(2000),
       {ok, State#state{stage=to_item}};
    to_item -> 
        lager:info("Populating items for dc ~w", [DcId]),
        %populate_items(TxServer, NumDCs, DcId, PartList),
        lists:foreach(fun({RepDcId, Replica}) -> populate_items(Replica, NumDCs, RepDcId, PartList) end, 
                  PopRange),
        {ok, State#state{stage=to_warehouse}};
    to_warehouse -> 
        lager:info("Populating warehouse for dc ~w", [DcId]),
        %WSeq = lists:seq(WPerDc*(DcId-1)+1, DcId*WPerDc),
        %lists:foreach(fun(WId) -> populate_warehouse(TxServer, WId, PartList, WPerDc) end, WSeq),
        lists:foreach(fun({RepDcId, Replica}) -> 
            RepWSeq = lists:seq(WPerDc*(RepDcId-1)+1, RepDcId*WPerDc),
            lists:foreach(fun(RepWId) -> populate_warehouse(Replica, RepWId, PartList, WPerDc) end, RepWSeq) end, 
                  PopRange),%RepIds),
        {ok, State#state{stage=to_stock}};
    to_stock ->
        lager:info("Populating stocks for dc ~w", [DcId]),
        %WSeq = lists:seq(WPerDc*(DcId-1)+1, DcId*WPerDc),
        %lists:foreach(fun(WId) -> populate_stock(TxServer, WId, PartList, WPerDc) end, WSeq),
        lists:foreach(fun({RepDcId, Replica}) -> 
            RepWSeq = lists:seq(WPerDc*(RepDcId-1)+1, RepDcId*WPerDc),
            lists:foreach(fun(RepWId) -> populate_stock(Replica, RepWId, PartList, WPerDc) end, RepWSeq) end, 
                 PopRange),%RepIds),
        {ok, State#state{stage=to_district, district_id=1}};
    to_district ->
        case DistrictId =< ?NB_MAX_DISTRICT of
            true ->
                lager:info("Populating districts ~w for dc ~w", [DistrictId, DcId]),
                %WSeq = lists:seq(WPerDc*(DcId-1)+1, DcId*WPerDc),
                %lists:foreach(fun(WId) -> populate_district(TxServer, WId, DistrictId, PartList, WPerDc) end, WSeq),
                lists:foreach(fun({RepDcId, Replica}) -> 
                    RepWSeq = lists:seq(WPerDc*(RepDcId-1)+1, RepDcId*WPerDc),
                    lists:foreach(fun(RepWId) -> populate_district(Replica, RepWId, DistrictId, PartList, WPerDc) end, RepWSeq) end, 
                    PopRange),%RepIds),
                {ok, State#state{district_id=DistrictId+1}};
            false ->
                {ok, State#state{stage=to_customer_order, district_id=1}}
        end;
    to_customer_order ->
        case DistrictId =< ?NB_MAX_DISTRICT of
            true ->
                lager:info("Populating customer orders for ~w", [DistrictId]),
                %WSeq = lists:seq(WPerDc*(DcId-1)+1, DcId*WPerDc),
                %lists:foreach(fun(WId) -> populate_customer_orders(TxServer, WId, DistrictId, PartList, WPerDc) end, WSeq),
                lists:foreach(fun({RepDcId, Replica}) -> 
                    RepWSeq = lists:seq(WPerDc*(RepDcId-1)+1, RepDcId*WPerDc),
                    lists:foreach(fun(RepWId) -> populate_customer_orders(Replica, RepWId, DistrictId, PartList, WPerDc) end, RepWSeq) end, 
                    PopRange),%RepIds),
                {ok, State#state{district_id=DistrictId+1}};
            false ->
		NowTime = os:timestamp(),
		UsedTime = get_time_diff(StartedTime, NowTime) div 1000000,
		lager:info("************** Node ~w finished populating, used ~w sec. ***************", [DcId, UsedTime]),
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
    DivItems = (?NB_MAX_ITEM - Remainder)/NumDCs,
    FirstItem = ((DcId-1) * DivItems) + 1,
    LastItem = case DcId of
                    NumDCs ->
                        DivItems + Remainder + FirstItem - 1;
                    _ ->
                        DivItems + FirstItem -1
                   end,
    put_range_items(TxServer, trunc(FirstItem), trunc(LastItem), DcId, PartList).

populate_warehouse(TxServer, WarehouseId, PartList, WPerDc)->
    random:seed({WarehouseId, 12,32}),
    Warehouse = tpcc_tool:create_warehouse(WarehouseId),
    WKey = tpcc_tool:get_key(Warehouse),
    WYtd = ?WAREHOUSE_YTD,
    WYtdKey = WKey++":w_ytd",
    lager:info("Populating ~p", [WKey]),
    lager:info("Populating ~p", [WYtdKey]),
    put_to_node(TxServer, to_dc(WarehouseId, WPerDc), PartList, WYtdKey, WYtd),
    put_to_node(TxServer, to_dc(WarehouseId, WPerDc), PartList, WKey, Warehouse).

populate_stock(TxServer, WarehouseId, PartList, WPerDc) ->
    random:seed({WarehouseId, 2,222}),
    Seq = lists:seq(1, ?NB_MAX_ITEM),
    lager:info("Warehouse ~w: Populating stocks from 1 to ~w", [WarehouseId, ?NB_MAX_ITEM]),
    DcId = to_dc(WarehouseId, WPerDc),
    
    FinalWS = lists:foldl(fun(StockId, WriteSet) ->
                      Stock = tpcc_tool:create_stock(StockId, WarehouseId),
                      Key = tpcc_tool:get_key(Stock),
                      defer_put(DcId, PartList, Key, Stock, WriteSet)
                      end, dict:new(), Seq),
    multi_put(TxServer, DcId, PartList, FinalWS).

populate_district(TxServer, WarehouseId, DistrictId, PartList, WPerDc) ->
    random:seed({WarehouseId, DistrictId, 22}),
    lager:info("Warehouse ~w: Populating district ~w", [WarehouseId, DistrictId]),
    DcId = to_dc(WarehouseId, WPerDc),
    District = tpcc_tool:create_district(DistrictId, WarehouseId),
    DKey = tpcc_tool:get_key(District),
    put_to_node(TxServer, DcId, PartList, DKey, District),
    DYtd = ?WAREHOUSE_YTD,
    YtdKey = DKey++":d_ytd",
    put_to_node(TxServer, DcId, PartList, YtdKey, DYtd).


populate_customer_orders(TxServer, WarehouseId, DistrictId, PartList, WPerDc) ->
    lager:info("Warehouse ~w, district ~w: Populating orders from 1 to ~w", [WarehouseId, DistrictId, 
                ?NB_MAX_ORDER]),
    NumUniqueNames = ?NUM_NAMES *?NUM_NAMES*?NUM_NAMES, 
    FCustomerSeq = lists:seq(1, NumUniqueNames),
    SCustomerSeq = lists:seq(NumUniqueNames+1, ?NB_MAX_CUSTOMER),
    %% Randomize the list of all combination of names
    FNameRandSeq = [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- FCustomerSeq])],
    SNameRandSeq = [tpcc_tool:c_last_rand(WarehouseId) || _N <- SCustomerSeq],
    random:seed({WarehouseId, DistrictId, 1111}),
    %% The list of customer
    OrderSeq = lists:seq(1, ?NB_MAX_ORDER),
    RandOrders = [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- OrderSeq])],
    {FRandOrders, SRandOrders} = lists:split(NumUniqueNames, RandOrders),
    [{"COMMIT_TIME", CommitTime}] = ets:lookup(meta_info, "COMMIT_TIME"),
    %% Magic number, assume that the order is created 1 sec ago.
    DcId = to_dc(WarehouseId, WPerDc),
    WS = add_customer_order(1, FNameRandSeq, FRandOrders, TxServer, WPerDc, DcId, PartList, WarehouseId, DistrictId, CommitTime, dict:new(), defer),
    multi_put(TxServer, DcId, PartList, WS),
    WS1 = add_customer_order(NumUniqueNames+1, SNameRandSeq, SRandOrders, TxServer, WPerDc, DcId, PartList, WarehouseId, DistrictId, CommitTime, dict:new(), direct),
    multi_put(TxServer, DcId, PartList, WS1).

add_customer_order(_, [], [], _, _, _, _, _, _, _, WS, _) ->
    WS; 
add_customer_order(CustomerId, [NameNum|RestName], [RandOrder|RestOrder], TxServer, WPerDc, DcId, PartList, WarehouseId, DistrictId, CommitTime, WS, Type) ->
    Date = CommitTime - 1000,
    HistoryTime = CommitTime - 123,
    CustomerTime = CommitTime - 13,

    CLast = tpcc_tool:name_by_num(NameNum),
    OrderId = RandOrder, 
    %lager:info("Customer id is ~w, order is ~w, clast is ~p", [CustomerId, RandOrder, CLast]),
    Customer = tpcc_tool:create_customer(WarehouseId, DistrictId, CustomerId, CLast, CustomerTime, OrderId),
    CKey = tpcc_tool:get_key(Customer),
    WS1 = defer_put(DcId, PartList, CKey, Customer, WS),
    CBalanceKey = CKey++":c_balance",
    WS2 = defer_put(DcId, PartList, CBalanceKey, -10, WS1),
    CLKey = tpcc_tool:get_key_by_param({WarehouseId, DistrictId, CLast}, customer_lookup),
    CustomerLookup = case Type of
                        direct -> read_from_node(TxServer, CLKey, DcId, PartList);
                        defer -> tpcc_tool:create_customer_lookup(WarehouseId, DistrictId, CLast)
                     end,
    Ids = CustomerLookup#customer_lookup.ids,
    CustomerLookup1 = CustomerLookup#customer_lookup{ids=[CustomerId|Ids]}, 
    WS3 = case Type of 
              direct -> put_to_node(TxServer, to_dc(WarehouseId, WPerDc), PartList, CLKey, CustomerLookup1), WS2;
              defer -> defer_put(DcId, PartList, CLKey, CustomerLookup1, WS2)
          end,
    History = tpcc_tool:create_history(WarehouseId, DistrictId, CustomerId, HistoryTime),
    HKey = tpcc_tool:get_key(History),
    WS4 = defer_put(DcId, PartList, HKey, History, WS3),

    %Date = tpcc_tool:now_nsec(),
    OOlCnt = tpcc_tool:random_num(5, 15),
    Order = tpcc_tool:create_order(WarehouseId, DistrictId, OrderId, OOlCnt, CustomerId,
        Date),
    OKey = tpcc_tool:get_key(Order),
    WS5 = defer_put(to_dc(WarehouseId, WPerDc), PartList, OKey, Order, WS4),
    populate_orderlines(TxServer, WarehouseId, DistrictId, OrderId, OOlCnt, Date, PartList, WPerDc),
    case OrderId >= ?LIMIT_ORDER of
        true ->
            NewOrder = tpcc_tool:create_neworder(WarehouseId, DistrictId, OrderId),
            NOKey = tpcc_tool:get_key(NewOrder),
            WS6 = defer_put(DcId, PartList, NOKey, NewOrder, WS5),
            add_customer_order(CustomerId+1, RestName, RestOrder, TxServer, WPerDc, DcId, PartList, WarehouseId, DistrictId, CommitTime, WS6, Type);
        false ->
            add_customer_order(CustomerId+1, RestName, RestOrder, TxServer, WPerDc, DcId, PartList, WarehouseId, DistrictId, CommitTime, WS5, Type)
    end.

populate_orderlines(TxServer, WarehouseId, DistrictId, OrderId, OOlCnt, Date, PartList, WPerDc) ->
    %lager:info("Warehouse ~w, district ~w: Populating orderlines from 1 to ~w", [WarehouseId, DistrictId, 
    %            OOlCnt]),
    Seq = lists:seq(1, OOlCnt),
    DcId = to_dc(WarehouseId, WPerDc),
    FinalWS = lists:foldl(fun(OrderlineId, WS) -> 
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
                    defer_put(DcId, PartList, OLKey, OrderLine, WS)
                end, dict:new(), Seq),
    multi_put(TxServer, DcId, PartList, FinalWS).
    
put_range_items(TxServer, FirstItem, LastItem, DcId, PartList) ->
    lager:info("Populating items from ~w to ~w", [FirstItem, LastItem]),
    Seq = lists:seq(FirstItem, LastItem),
    FinalWS = lists:foldl(fun(ItemId, WS) ->
                    Item = tpcc_tool:create_item(ItemId),
                    Key = tpcc_tool:get_key(Item),
                    defer_put(DcId, PartList, Key, Item, WS)
                    end, dict:new(), Seq),
    multi_put(TxServer, DcId, PartList, FinalWS).

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
    put(TxServer, Partition1, [{K1, C_C_LAST}], COMMIT_TIME),
    put(TxServer, Partition2, [{K2, C_C_ID}], COMMIT_TIME),
    put(TxServer, Partition3, [{K3, C_OL_I_ID}], COMMIT_TIME),
    put(TxServer, Partition4, [{K4, COMMIT_TIME}], COMMIT_TIME).

get_partition(Key, PartList, HashLength) ->
    Num = crypto:bytes_to_integer(erlang:md5(Key)) rem HashLength +1,
    lists:nth(Num, PartList).
    
put_to_node(TxServer, DcId, PartList, Key, Value) ->
    {_, L} = lists:nth(DcId, PartList),
    Index = crypto:bytes_to_integer(erlang:md5(Key)) rem length(L) + 1,
    Part = lists:nth(Index, L),    
    [{"COMMIT_TIME", CommitTime}] = ets:lookup(meta_info, "COMMIT_TIME"),
    put(TxServer, Part, [{Key, Value}], CommitTime).

defer_put(DcId, PartList, Key, Value, WriteSet) ->
    {_, L} = lists:nth(DcId, PartList),
    Index = crypto:bytes_to_integer(erlang:md5(Key)) rem length(L) + 1,
    dict:append(Index, {Key, Value}, WriteSet).

multi_put(TxServer, DcId, PartList, WriteSet) ->
    {_, L} = lists:nth(DcId, PartList),
    DictList = dict:to_list(WriteSet),
    [{"COMMIT_TIME", CommitTime}] = ets:lookup(meta_info, "COMMIT_TIME"),
    lists:foreach(fun({Index, KeyValues}) ->
            Part = lists:nth(Index, L),    
            put(TxServer, Part, KeyValues, CommitTime)
            end, DictList).              

put(TxServer, Part, KeyValues, CommitTime) ->
    %lager:info("Single Puting [~p, ~p] to ~w", [Key, Value, Part]),
    case TxServer of
        {server, S} ->
            {ok, _} = tx_cert_sup:append_values(S, Part, KeyValues, CommitTime);
        {rep, S} ->
            ok = data_repl_serv:append_values(S, KeyValues, CommitTime)
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
            %lager:error("Key ~p not found!!!!", [Key]),
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
    %lager:info("Trying to get index: PL ~w, List ~w", [PL, List]),
    [index(X, List) || X <- PL ].

read({server, TxServer}, Key, ExpandPartList, HashLength) ->
    Part = get_partition(Key, ExpandPartList, HashLength),
    {ok, V} = tx_cert_sup:single_read(TxServer, Key, Part),
    case V of
        [] ->
            %lager:error("Key ~p not found!!!!", [Key]),
            error;
        _ ->
            V
    end.

get_rep_name(Target, Rep) ->
    list_to_atom(atom_to_list(Target)++"repl"++atom_to_list(Rep)).

get_time_diff({A1, B1, C1}, {A2, B2, C2}) ->
    ((A2-A1)*1000000+ (B2-B1))*1000000+ C2-C1.

to_dc(WId, WPerDc) ->
    (WId-1) div WPerDc + 1.
