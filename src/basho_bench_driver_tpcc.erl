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
-module(basho_bench_driver_tpcc).

-export([new/1,
         run/4]).

-include("basho_bench.hrl").
-include("tpcc.hrl").

-define(TIMEOUT, 20000).

-record(state, {worker_id,
                time,
                part_list,
                expand_part_list,
                hash_length,
                c_c_last,
                c_c_id,
                c_ol_i_id,
                my_rep_list,
                no_rep_list,
                my_rep_ids,
                no_rep_ids,
                item_ranges,
                num_dcs,
                access_master,
                access_slave,
                dc_id,
                tx_server,
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
    %_PbPorts = basho_bench_config:get(antidote_pb_port),
    MyNode = basho_bench_config:get(antidote_mynode),
    Cookie = basho_bench_config:get(antidote_cookie),
    IPs = basho_bench_config:get(antidote_pb_ips),
    ToSleep = basho_bench_config:get(to_sleep),
   
    AccessMaster = basho_bench_config:get(access_master),
    AccessSlave = basho_bench_config:get(access_slave),

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
    lager:info("MyTxServer is ~w", [MyTxServer]),
    {PartList, ReplList} =  rpc:call(TargetNode, hash_fun, get_hash_fun, []), 
    lager:info("Part list is ~w, repl list is ~w", [PartList, ReplList]),

    [M] = [L || {N, L} <- ReplList, N == TargetNode ],
    AllDcs = [N || {N, _} <- PartList],
    MyRepIds = get_indexes(M, AllDcs),
    MyRepList = [{N, get_rep_name(TargetNode, lists:nth(N, AllDcs))} || N <- MyRepIds],
    lager:info("My Rep Ids is ~p, my rep list is ~p", [MyRepIds, MyRepList]),
    DcId = index(TargetNode, AllDcs),
    NumDcs = length(AllDcs),
    lager:info("All Dcs is ~p, dc id is ~w", [AllDcs, DcId]),
    NoRepList = ((AllDcs -- MyRepList)) -- [TargetNode],
    NoRepIds = get_indexes(NoRepList, AllDcs),
    lager:info("NoRep list is ~w, no rep ids is ~w", [NoRepList, NoRepIds]),
    %ets:new(dep_table, [private, named_table, bag]),

    ExpandPartList = lists:flatten([L || {_, L} <- PartList]),
    %lager:info("Ex list is ~w", [ExpandPartList]),
    HashLength = length(ExpandPartList),

    lager:info("Part list is ~w",[PartList]),
    timer:sleep(ToSleep),
    TxId = gen_server:call({global, MyTxServer}, {start_tx}),
    C_C_LAST = read(MyTxServer, TxId, "C_C_LAST", ExpandPartList, HashLength),
    C_C_ID = read(MyTxServer, TxId, "C_C_ID", ExpandPartList, HashLength),
    C_OL_I_ID = read(MyTxServer, TxId, "C_OL_I_ID", ExpandPartList, HashLength),
    ItemRanges = init_item_ranges(NumDcs, ?NB_MAX_ITEM),
    lager:info("Cclast ~w, ccid ~w, coliid ~w", [C_C_LAST, C_C_ID, C_OL_I_ID]),
    {ok, #state{time={1,1,1}, worker_id=Id,
               tx_server=MyTxServer,
               access_master=AccessMaster,
               access_slave=AccessSlave,
               part_list = PartList,
               my_rep_list = MyRepList,
               my_rep_ids = MyRepIds,
               no_rep_list = NoRepList,
               no_rep_ids = NoRepIds,
               item_ranges = ItemRanges,
               expand_part_list = ExpandPartList,
               hash_length = HashLength,   
               c_c_last = C_C_LAST,
               c_c_id = C_C_ID,
               c_ol_i_id = C_OL_I_ID, 
               num_dcs = NumDcs,
               dc_id = DcId,
               target_node=TargetNode}}.

%% @doc Read a key
run(new_order, _KeyGen, _ValueGen, State=#state{part_list=PartList, tx_server=TxServer, 
        my_rep_ids=MyRepIds, my_rep_list=MyRepList, 
        no_rep_ids=NoRepIds, dc_id=DcId, 
        item_ranges=ItemRanges, c_c_id=C_C_ID, c_ol_i_id=C_OL_I_ID, access_master=AccessMaster, access_slave=AccessSlave}) ->
    RS = dict:new(),
    WS = dict:new(),
    %LocalWS = dict:new(),
    %RemoteWS = dict:new(),

	%% TODO: maybe need to change warehouse
	WarehouseId = DcId,
    DistrictId = tpcc_tool:random_num(1, ?NB_MAX_DISTRICT),
    CustomerId = tpcc_tool:non_uniform_random(C_C_ID, ?A_C_ID, 1, ?NB_MAX_CUSTOMER),
    NumItems = tpcc_tool:random_num(?MIN_ITEM, ?MAX_ITEM),
    %lager:info("DistrictId is ~w, Customer Id is ~w, NumItems is ~w", [DistrictId, CustomerId, NumItems]),

    %TxId = {tx_id, tpcc_tool:now_nsec(), self()}, %,gen_server:call({global, TxServer}, {start_tx}),
    TxId = gen_server:call({global, TxServer}, {start_tx}),
    %lager:info("TxId is ~w", [TxId]),
    CustomerKey = tpcc_tool:get_key_by_param({WarehouseId, DistrictId, CustomerId}, customer), 
    _Customer = read_from_node(TxServer, TxId, CustomerKey, WarehouseId, PartList, MyRepList), 
    WarehouseKey = tpcc_tool:get_key_by_param({WarehouseId}, warehouse),
    _Warehouse = read_from_node(TxServer, TxId, WarehouseKey, WarehouseId, PartList, MyRepList),

    DistrictKey = tpcc_tool:get_key_by_param({WarehouseId, DistrictId}, district),
    District = read_from_node(TxServer, TxId, DistrictKey, WarehouseId, PartList, MyRepList),
    OId = District#district.d_next_o_id,

    NewOrder = tpcc_tool:create_neworder(WarehouseId, DistrictId, OId),
    NewOrderKey = tpcc_tool:get_key_by_param({WarehouseId, DistrictId, OId}, neworder),
    WS1 = dict:store({WarehouseId, NewOrderKey}, NewOrder, WS), 
    %LocalWS1 = add_to_writeset(NewOrderKey, NewOrder, lists:nth(DcId, PartList), LocalWS),
    District1 = District#district{d_next_o_id=OId+1},
    %LocalWS2 = add_to_writeset(DistrictKey, District1, lists:nth(DcId, PartList), LocalWS1),
    WS2 = dict:store({WarehouseId, DistrictKey}, District1, WS1), 

    Seq = lists:seq(1, NumItems),
    {WS3, _, AllLocal} = lists:foldl(fun(OlNumber, {TWS, TRS, AL}) ->
                    WId = pick_warehouse(WarehouseId, MyRepIds, NoRepIds, AccessMaster, AccessSlave),
                    {Min, Max} = lists:nth(WId, ItemRanges),
                    %ItemId = case tpcc_tool:random_num(1, 100) of
                    %            1 ->
                    %                -12345;
                    %            _ ->
                    ItemId = tpcc_tool:non_uniform_random(C_OL_I_ID, ?A_OL_I_ID, Min, Max),
                             %end,
                    Quantity = tpcc_tool:random_num(1, ?NB_MAX_DISTRICT),
                    ItemKey = tpcc_tool:get_key_by_param({ItemId}, item),
                    %Item = read_from_node(TxServer, TxId, ItemKey, WId, PartList, MyRepList),
                    {Item, TRS1} = read_from_cache_or_node(TRS, TxServer, TxId, ItemKey, WId, PartList, MyRepList),
                    StockKey = tpcc_tool:get_key_by_param({WId, ItemId}, stock),
                    %Stock = read_from_node(TxServer, TxId, StockKey, WId, PartList, MyRepList),
                    {Stock, TRS2} = read_from_cache_or_node(TRS1, TxServer, TxId, StockKey, WId, PartList, MyRepList),
                    NewSQuantity = case Stock#stock.s_quantity - Quantity >= 10 of
                                        true -> Stock#stock.s_quantity - Quantity;
                                        false -> Stock#stock.s_quantity - Quantity + 91
                                    end,
                    SRemote = case WId of 
                                    WarehouseId -> Stock#stock.s_remote_cnt;
                                    _ -> Stock#stock.s_remote_cnt+1
                                end,
                    SYtd = Stock#stock.s_ytd,
                    SOrderCnt = Stock#stock.s_order_cnt,
                    Stock1 = Stock#stock{s_quantity=NewSQuantity, s_ytd=SYtd+Quantity, s_remote_cnt=SRemote,
                                   s_order_cnt=SOrderCnt+1},                    
                    {TWS3, TRS3} = add_to_wr_set(WId, StockKey, Stock1, TWS, TRS2),
                    %{LWS1, RWS1} = case WId of
                    %                    DcId -> {add_to_writeset(StockKey, Stock1, lists:nth(DcId, PartList), LWS), RWS};
                    %                    _ -> {LWS, add_to_writeset(StockKey, Stock1, lists:nth(WId, PartList), RWS)}
                    %                end,
                    OlAmount = Quantity * Item#item.i_price,
                    %IData = Item#item.i_data,
                    %SData = Stock#stock.s_data,;
                    OlDistInfo = get_district_info(Stock1, DistrictId),
                    Orderline = tpcc_tool:create_orderline(WarehouseId, DistrictId, WId, OId, ItemId, 
                        OlNumber, Quantity, OlAmount, OlDistInfo),
                    %LWS2 = add_to_writeset(tpcc_tool:get_key(Orderline), Orderline, lists:nth(DcId, PartList), LWS1),
                    TWS4 = dict:store({WarehouseId, tpcc_tool:get_key(Orderline)}, Orderline, TWS3),
                    AL1 = case WId of 
                                    WarehouseId -> AL;
                                    _ -> 0 
                                end,
                    {TWS4, TRS3, AL1}
            end, {WS2, RS, 1}, Seq),
    Order = tpcc_tool:create_order(WarehouseId, DistrictId, OId, NumItems, CustomerId, tpcc_tool:now_nsec(), AllLocal), 
    OrderKey = tpcc_tool:get_key_by_param({WarehouseId, DistrictId, OId}, order),
    WS4 = dict:store({WarehouseId, OrderKey}, Order, WS3),
    %LocalWS4 = add_to_writeset(OrderKey, Order, lists:nth(DcId, PartList), LocalWS3),
    {LocalWriteList, RemoteWriteList} = get_local_remote_writeset(WS4, PartList, DcId),
    %lager:info("Local Write set is ~p", [LocalWriteList]),
    %lager:info("Remote Write set is ~p", [RemoteWriteList]),
    %DepsList = ets:lookup(dep_table, TxId),
    Response =  gen_server:call({global, TxServer}, {certify, TxId, LocalWriteList, RemoteWriteList}),%, length(DepsList)}),
    case Response of
        {ok, _Value} ->
            {ok, State};
        {error,timeout} ->
            lager:info("Timeout on client ~p",[TxServer]),
            {error, timeout, State};
        {aborted, _} ->
            %lager:error("Aborted"),
            {error, aborted, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end;

%% @doc Payment transaction of TPC-C
run(payment, _KeyGen, _ValueGen, State=#state{part_list=PartList, tx_server=TxServer,
        my_rep_list=MyRepList,
        dc_id=DcId, num_dcs=NumDcs,
        c_c_id=C_C_ID, c_c_last = C_C_LAST, access_master=AccessMaster}) ->
    WS = dict:new(),
    %LocalWS = dict:new(),
    %RemoteWS = dict:new(),
	TWarehouseId = DcId,
	DistrictId = tpcc_tool:random_num(1, ?NB_MAX_DISTRICT),
	
    %% TODO: Should be 85 in the original impl
    {CWId, CDId} = case tpcc_tool:random_num(1, 100) =< AccessMaster of
				        true ->
				            {TWarehouseId, DistrictId};
				        false ->
							RId = tpcc_tool:random_num(1, ?NB_MAX_DISTRICT),
				    		N = tpcc_tool:random_num(1, NumDcs-1),
							case N >= TWarehouseId of
								true -> {N+1, RId};  false -> {N, RId}
							end
				  	end,
	PaymentAmount = tpcc_tool:random_num(100, 500000) / 100.0,

	%TxId = {tx_id, tpcc_tool:now_nsec(), self()},	
    TxId = gen_server:call({global, TxServer}, {start_tx}),
	WarehouseKey = tpcc_tool:get_key_by_param({TWarehouseId}, warehouse),
    Warehouse = read_from_node(TxServer, TxId, WarehouseKey, TWarehouseId, PartList, MyRepList),
	WYtdKey = WarehouseKey++":w_ytd",
	WYtd = read_from_node(TxServer, TxId, WYtdKey, TWarehouseId, PartList, MyRepList),
	WYtd1 = WYtd+ PaymentAmount,
	WS1 = dict:store({TWarehouseId, WYtdKey}, WYtd1, WS),
	DistrictKey = tpcc_tool:get_key_by_param({TWarehouseId, DistrictId}, district),
    District = read_from_node(TxServer, TxId, DistrictKey, DcId, PartList, MyRepList),
	DYtdKey = DistrictKey++":d_ytd",
	DYtd = read_from_node(TxServer, TxId, DYtdKey, TWarehouseId, PartList, MyRepList),
	DYtd1 = DYtd+ PaymentAmount,
	WS2 = dict:store({TWarehouseId, DYtdKey}, DYtd1, WS1),
	
		%% 60% change to load customer by name, otherwise not by name
    CW = case tpcc_tool:random_num(1, 100) =< 60 of
			true ->
				Rand = trunc(tpcc_tool:non_uniform_random(C_C_LAST, ?A_C_LAST, 0, ?MAX_C_LAST)),
	         	CLastName = tpcc_tool:last_name(Rand),
				CustomerLookupKey = tpcc_tool:get_key_by_param({CWId, CDId, CLastName}, customer_lookup),
				CustomerLookup = read_from_node(TxServer, TxId, CustomerLookupKey, CWId, PartList, MyRepList),
                case CustomerLookup of
                    error ->
                        error;                    
                    _ ->
                        Ids = CustomerLookup#customer_lookup.ids,
                        Customers= lists:foldl(fun(Id, Acc) ->
                                    CKey = tpcc_tool:get_key_by_param({CWId, CDId, Id}, customer),
                                    C = read_from_node(TxServer, TxId, CKey, CWId, PartList, MyRepList),
                                    case C of
                                        error -> Acc;  _ -> [C|Acc]
                                    end end, [], Ids),
                        SortedCustomers = lists:sort(Customers),
                        Middle = (length(Customers) + 1) div 2,
                        lists:nth(Middle, SortedCustomers)
                end;
	       	false ->
	         	CustomerID = tpcc_tool:non_uniform_random(C_C_ID, ?A_C_ID, 1, ?NB_MAX_CUSTOMER),
				CKey = tpcc_tool:get_key_by_param({CWId, CDId, CustomerID}, customer),
				read_from_node(TxServer, TxId, CKey, CWId, PartList, MyRepList)
		end,
    case CW of
        error ->
            {error, not_found, State};
        _ ->
            CWBalanceKey = tpcc_tool:get_key(CW)++":c_balance",
            CWBalance = read_from_node(TxServer, TxId, CWBalanceKey, TWarehouseId, PartList, MyRepList),
            CWBalance1 = CWBalance + PaymentAmount,
            WS3 = dict:store({CWId, CWBalanceKey}, CWBalance1, WS2),
            WName = Warehouse#warehouse.w_name,
            DName = District#district.d_name,
            HData = lists:sublist(WName, 1, 10) ++ "  " ++ lists:sublist(DName, 1, 10),
            History = tpcc_tool:create_history(TWarehouseId, DistrictId, CWId, CDId, 
                                               CW#customer.c_id, tpcc_tool:now_nsec(), PaymentAmount, HData),
            HistoryKey = tpcc_tool:get_key(History),
            WS4 = dict:store({CWId, HistoryKey}, History, WS3),
            {LocalWriteList, RemoteWriteList} = get_local_remote_writeset(WS4, PartList, DcId),
            %DepsList = ets:lookup(dep_table, TxId),
            Response =  gen_server:call({global, TxServer}, {certify, TxId, LocalWriteList, RemoteWriteList}),%, length(DepsList)}),
            case Response of
                {ok, _Value} ->
                    {ok, State};
                {error,timeout} ->
                    lager:info("Timeout on client ~p",[TxServer]),
                    {error, timeout, State};
                {aborted, _} ->
                    %lager:error("Aborted"),
                    {error, aborted, State};
                {badrpc, Reason} ->
                    {error, Reason, State}
            end
    end;

%% @doc Payment transaction of TPC-C
run(order_status, _KeyGen, _ValueGen, State=#state{part_list=PartList, tx_server=TxServer,
        my_rep_list=MyRepList,
        dc_id=DcId,
        c_c_id=C_C_ID, c_c_last = C_C_LAST}) ->

	TWarehouseId = DcId,
	DistrictId = tpcc_tool:random_num(1, ?NB_MAX_DISTRICT),
	
	%TxId = {tx_id, tpcc_tool:now_nsec(), self()},
    TxId = gen_server:call({global, TxServer}, {start_tx}),
	CW = case tpcc_tool:random_num(1, 100) =< 60 of
			true ->
				Rand = trunc(tpcc_tool:non_uniform_random(C_C_LAST, ?A_C_LAST, 0, ?MAX_C_LAST)),
	         	CLastName = tpcc_tool:last_name(Rand),
				CustomerLookupKey = tpcc_tool:get_key_by_param({TWarehouseId, DistrictId, CLastName}, customer_lookup),
				CustomerLookup = read_from_node(TxServer, TxId, CustomerLookupKey, TWarehouseId, PartList, MyRepList),
                case CustomerLookup of
                    error ->
                        lager:error("Key not found by last name ~w", [CLastName]),
                        error;
                    _ ->
                        Ids = CustomerLookup#customer_lookup.ids,
                        Customers= lists:foldl(fun(Id, Acc) ->
                                    CKey = tpcc_tool:get_key_by_param({TWarehouseId, DistrictId, Id}, customer),
                                    C = read_from_node(TxServer, TxId, CKey, TWarehouseId, PartList, MyRepList),
                                    case C of
                                        error -> Acc;  _ -> [C|Acc]
                                    end end, [], Ids),
                        SortedCustomers = lists:sort(Customers),
                        Middle = (length(Customers) + 1) div 2,
                        %lager:info("Loading by Id... Got ~w keys", [length(Customers)]),
                        lists:nth(Middle, SortedCustomers)
                end;
	       	false ->
	         	CustomerID = tpcc_tool:non_uniform_random(C_C_ID, ?A_C_ID, 1, ?NB_MAX_CUSTOMER),
				CKey = tpcc_tool:get_key_by_param({TWarehouseId, DistrictId, CustomerID}, customer),
				read_from_node(TxServer, TxId, CKey, TWarehouseId, PartList, MyRepList)
		end,
    case CW of
        error ->
            lager:error("Key not found!"),
            {error, not_found, State};
        _ ->
            CWId = CW#customer.c_id,
            Seq = lists:seq(1, ?NB_MAX_ORDER),
            OrderList = lists:foldl(fun(Id, Acc) ->
                                        OrdKey = tpcc_tool:get_key_by_param({TWarehouseId, DistrictId, Id}, order),
                                        Ord = read_from_node(TxServer, TxId, OrdKey, TWarehouseId, PartList, MyRepList),
                                        case Ord#order.o_c_id of
                                            CWId -> [Ord|Acc]; _ -> Acc
                                        end end, [], Seq),
            %lager:info("CWId is ~w, length of orderlist is ~w", [CWId, length(OrderList)]),
            case OrderList of
                [] ->
                    lager:info("Found nothing!");
                _ ->
                    Sorted = lists:sort(OrderList),
                    LastOne = lists:nth(length(Sorted), Sorted),
                    NumLines = LastOne#order.o_ol_cnt,
                    Seq2 = lists:seq(1, NumLines),
                    %lager:info("Loading ~w orderlines", [NumLines]),
                    OWId = LastOne#order.o_w_id,
                    ODId = LastOne#order.o_d_id,
                    OId = LastOne#order.o_id,
                    lists:foreach(fun(Number) ->
                            OlKey = tpcc_tool:get_key_by_param({OWId, ODId, OId, Number}, orderline),
                            _Ol = read_from_node(TxServer, TxId, OlKey, TWarehouseId, PartList, MyRepList)
                            end, Seq2)
            end,
            {ok, State}
    end.

get_partition(Key, PartList, HashLength) ->
    Num = crypto:bytes_to_integer(erlang:md5(Key)) rem HashLength +1,
    lists:nth(Num, PartList).
    
index(Elem, L) ->
    index(Elem, L, 1).

index(_, [], _) ->
    -1;
index(E, [E|_], N) ->
    N;
index(E, [_|L], N) ->
    index(E, L, N+1).

read_from_node(TxServer, TxId, Key, DcId, PartList, MyRepList) ->
    {ok, V} = case get_replica(DcId, MyRepList) of
                    false ->
                        {_, L} = lists:nth(DcId, PartList),
                        Index = crypto:bytes_to_integer(erlang:md5(Key)) rem length(L) + 1,
                        Part = lists:nth(Index, L),
                        %{I, Node} = Part, 
                        %Req = {riak_vnode_req_v1, I, {server, undefined, undefined}, Index},
                        %gen_server:call({clocksi_vnode_master, Node}, Req, infinity);
                        tx_cert_sup:read(TxServer, TxId, Key, Part);
                    N ->
                        data_repl_server:read(N, TxId, Key)
                end,
    %case Res of
    %    {specula, DepTx} ->
    %        ets:insert(dep_table, {TxId, DepTx});
    %    ok ->
    %        ok
    %end,
    case V of
        [] ->
            lager:error("Key ~p not found!!!!", [Key]),
            error;
        _ ->
            V
    end.

read_from_cache_or_node(ReadSet, TxServer, TxId, Key, DcId, PartList, MyRepList) ->
    case dict:find(Key, ReadSet) of
        {ok, V} ->
            %lager:info("In read set..Key ~p, V ~p, Readset ~p", [Key, V, ReadSet]),
            {V, ReadSet};
        error ->
            V = read_from_node(TxServer, TxId, Key, DcId, PartList, MyRepList),
            ReadSet1 = dict:store(Key, V, ReadSet),
            %lager:info("Not in read set..Key ~p, V ~p, Readset ~p", [Key, V, ReadSet1]),
            {V, ReadSet1}
    end.
 
read(TxServer, TxId, Key, ExpandPartList, HashLength) ->
    Part = get_partition(Key, ExpandPartList, HashLength),
    {ok, V} = tx_cert_sup:read(TxServer, TxId, Key, Part),
    %case Res of
    %    {specula, DepTx} ->
    %        ets:insert(dep_table, {TxId, DepTx});
    %    ok ->
    %        ok
    %end,
    case V of
        [] ->
            lager:error("Key ~p not found!!!!", [Key]),
            error;
        _ ->
            %lager:info("Reading ~p, ~p", [Key, V]),
            V
    end.

get_local_remote_writeset(WriteSet, PartList, LocalId) ->
    {LWSD, RWSD} = dict:fold(fun({Id, Key}, Value, {LWS, RWS}) ->
                    case Id of LocalId -> {add_to_writeset(Key, Value, lists:nth(LocalId, PartList), LWS), RWS};
                               _ -> {LWS, add_to_writeset(Key, Value, lists:nth(Id, PartList), RWS)}
                    end end, {dict:new(), dict:new()}, WriteSet),
    {dict:to_list(LWSD), dict:to_list(RWSD)}.


add_to_wr_set(DcId, Key, Value, WriteSet, ReadSet) ->
    WriteSet1 = dict:store({DcId, Key}, Value, WriteSet),
    ReadSet1 = dict:store(Key, Value, ReadSet),
    {WriteSet1, ReadSet1}.    

add_to_writeset(Key, Value, {_, PartList}, WSet) ->
    Index = crypto:bytes_to_integer(erlang:md5(Key)) rem length(PartList) + 1,
    Part = lists:nth(Index, PartList),
    %lager:info("Adding  ~p, ~p to ~w", [Key, Value, Part]),
    dict:append(Part, {Key, Value}, WSet).

get_indexes(PL, List) ->
    lager:info("Trying to get index: PL ~w, List ~w", [PL, List]),
    [index(X, List) || X <- PL ].

pick_warehouse(MyId, RepIds, NoRepIds, AccessMaster, AccessRep) ->
    R = random:uniform(100),
    case R =< AccessMaster of
        true ->
            MyId;
        false ->
            case R =< AccessMaster + AccessRep of
                true ->
                    lists:nth(R rem length(RepIds)+1, RepIds);
                false ->
                    lists:nth(R rem length(NoRepIds)+1, NoRepIds)
            end
    end.

init_item_ranges(NumDCs, Max) ->
    Remainder = Max rem NumDCs,
    DivItems = (Max-Remainder)/NumDCs,
    Seq = lists:seq(1, NumDCs),
    lists:foldl(fun(N, Acc) ->
                    FirstItem = ((N-1) * DivItems) + 1,
                    LastItem = case N of
                                    NumDCs ->
                                        DivItems + Remainder + FirstItem - 1;
                                    _ ->
                                        DivItems + FirstItem -1
                                end,
                    Acc++[{trunc(FirstItem), trunc(LastItem)}] 
                    end, [], Seq).
    
get_district_info(Stock, 1) ->
    Stock#stock.s_dist_01;
get_district_info(Stock, 2) ->
    Stock#stock.s_dist_02;
get_district_info(Stock, 3) ->
    Stock#stock.s_dist_03;
get_district_info(Stock, 4) ->
    Stock#stock.s_dist_04;
get_district_info(Stock, 5) ->
    Stock#stock.s_dist_05;
get_district_info(Stock, 6) ->
    Stock#stock.s_dist_06;
get_district_info(Stock, 7) ->
    Stock#stock.s_dist_07;
get_district_info(Stock, 8) ->
    Stock#stock.s_dist_08;
get_district_info(Stock, 9) ->
    Stock#stock.s_dist_09;
get_district_info(Stock, 10) ->
    Stock#stock.s_dist_10.

get_replica(_, []) ->
    false;
get_replica(E, [{E, N}|_]) ->
    N;
get_replica(E, [_|L]) ->
    get_replica(E, L).

get_rep_name(Target, Rep) ->
    list_to_atom(atom_to_list(Target)++"repl"++atom_to_list(Rep)).



    
