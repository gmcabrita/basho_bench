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
        terminate/2,
	read/5,
         run/4]).

-include("basho_bench.hrl").
-include("tpcc.hrl").

-define(TIMEOUT, 10000).

-record(state, {worker_id,
                time,
                part_list,
                expand_part_list,
                hash_length,
                w_per_dc,
                c_c_last,
                c_c_id,
                c_ol_i_id,
                my_rep_list,
                my_table,
                to_sleep,
                hash_dict,
                no_rep_list,
                my_rep_ids,
                no_rep_ids,
                item_ranges,
                num_nodes,
                access_master,
                access_slave,
		        no_specula,
                no_local,
                no_remote,
                no_local_abort,
                no_remote_abort,
		        no_read,
                new_order_committed,
                payment_prep,
		        payment_read,
                node_id,
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
    MasterToSleep = basho_bench_config:get(master_to_sleep),
   
    AccessMaster = basho_bench_config:get(access_master),
    AccessSlave = basho_bench_config:get(access_slave),
    WPerNode = basho_bench_config:get(w_per_dc),

    case net_kernel:start(MyNode) of
        {ok, _} ->
	        ok;
            %?INFO("Net kernel started as ~p\n", [node()]);
        {error, {already_started, _}} ->
            ?INFO("Net kernel already started as ~p\n", [node()]),
            ok;
        {error, Reason} ->
            ?FAIL_MSG("Failed to start net_kernel for ~p: ~p\n", [?MODULE, Reason])
    end,

    %% Choose the node using our ID as a modulus
    TargetNode = lists:nth((Id rem length(IPs)+1), IPs),
    true = erlang:set_cookie(node(), Cookie),

    %?INFO("Using target node ~p for worker ~p\n", [TargetNode, Id]),
    _Result = net_adm:ping(TargetNode),
    %?INFO("Result of ping is ~p \n", [Result]),

    {PartList, ReplList, NumDcs} =  rpc:call(TargetNode, hash_fun, get_hash_fun, []), 
    lager:info("NumDcs is ~w", [NumDcs]),
    %lager:info("Part list is ~w, repl list is ~w", [PartList, ReplList]),

    [M] = [L || {N, L} <- ReplList, N == TargetNode ],
    AllNodes = [N || {N, _} <- PartList],
    MyRepIds = get_indexes(M, AllNodes),
    %lager:info("My Rep Ids is ~p, my rep list is ~p", [MyRepIds, MyRepList]),
    NodeId = index(TargetNode, AllNodes),
    NumNodes = length(AllNodes),
    MyTxServer = list_to_atom(atom_to_list(TargetNode) ++ "-cert-" ++ integer_to_list((Id-1) div length(IPs)+1)),
    %lager:info("MyTxServer is ~w", [MyTxServer]),

    %lager:info("All Dcs is ~p, dc id is ~w", [AllDcs, DcId]),
    NoRepList = (AllNodes -- M) -- [TargetNode],
    NoRepIds = get_indexes(NoRepList, AllNodes),
    HashDict = build_local_norep_dict(NodeId, AllNodes, NoRepIds, NumDcs),
    HashDict1 =  lists:foldl(fun(N, D) ->
                        dict:store(N, get_rep_name(TargetNode, lists:nth(N, AllNodes)), D)
                        end, HashDict, MyRepIds),
    lager:info("My Rep Ids is ~p, my rep list is ~p", [MyRepIds, dict:to_list(HashDict1)]),
    lager:info("NoRep list is ~w, no rep ids is ~w", [NoRepList, NoRepIds]),

    ExpandPartList = lists:flatten([L || {_, L} <- PartList]),
    %lager:info("Ex list is ~w", [ExpandPartList]),
    HashLength = length(ExpandPartList),

    %lager:info("Part list is ~w",[PartList]),
    MyTable =ets:new(my_table, [private, set]),
    ets:insert(MyTable, {payment, 0,0,0}),
    ets:insert(MyTable, {new_order, 0,0,0}),
    case Id of 1 -> timer:sleep(MasterToSleep);
    	       _ -> timer:sleep(ToSleep)
    end,
    Key1 = "C_C_LAST",
    Key2 = "C_C_ID",
    Key3 = "C_OL_I_ID",
    Part1 = get_partition(Key1, ExpandPartList, HashLength),
    Part2 = get_partition(Key2, ExpandPartList, HashLength),
    Part3 = get_partition(Key3, ExpandPartList, HashLength),
    {ok, C_C_LAST} = rpc:call(TargetNode, tx_cert_sup, single_read, [MyTxServer, Key1, Part1]),
    {ok, C_C_ID} = rpc:call(TargetNode, tx_cert_sup, single_read, [MyTxServer, Key2, Part2]),
    {ok, C_OL_I_ID} = rpc:call(TargetNode, tx_cert_sup, single_read, [MyTxServer, Key3, Part3]),
    ItemRanges = init_item_ranges(NumNodes, ?NB_MAX_ITEM),
    lager:info("Cclast ~w, ccid ~w, coliid ~w", [C_C_LAST, C_C_ID, C_OL_I_ID]),
    {ok, #state{time={1,1,1}, worker_id=Id,
               tx_server=MyTxServer,
               access_master=AccessMaster,
               access_slave=AccessSlave,
               part_list = PartList,
               hash_dict = HashDict1,
               w_per_dc=WPerNode,
               my_table=MyTable,
               my_rep_ids = MyRepIds,
               no_rep_list = NoRepList,
               to_sleep=ToSleep,
               no_specula={0,0},
               no_local={0,0},
               no_remote={0,0},
               no_local_abort={0,0},
               no_remote_abort={0,0},
               no_read=0,
               payment_prep={0,0}, 
               payment_read={0,0}, 
               no_rep_ids = NoRepIds,
               item_ranges = ItemRanges,
               expand_part_list = ExpandPartList,
               hash_length = HashLength,   
               c_c_last = C_C_LAST,
               c_c_id = C_C_ID,
               c_ol_i_id = C_OL_I_ID, 
               num_nodes = NumNodes,
               node_id = NodeId,
               target_node=TargetNode}}.

%% @doc Warehouse, District are always local.. Only choose to access local or remote objects when reading
%% objects. 
run(new_order, _KeyGen, _ValueGen, State=#state{part_list=PartList, tx_server=TxServer, 
        my_rep_ids=MyRepIds, hash_dict=HashDict, no_rep_ids=NoRepIds, node_id=DcId, 
        no_local=NOLocal, no_remote=NORemote, no_local_abort=NOLAbort, worker_id=WorkerId,
        no_read=NORead, no_remote_abort=NORAbort, no_specula=NOSpecula, w_per_dc=WPerNode, my_table=MyTable, 
        item_ranges=ItemRanges, c_c_id=C_C_ID, c_ol_i_id=C_OL_I_ID, access_master=AccessMaster, access_slave=AccessSlave}) ->
    RS = dict:new(),
    WS = dict:new(),
    %LocalWS = dict:new(),
    %RemoteWS = dict:new(),

	%% TODO: maybe need to change warehouse
    WarehouseId = WPerNode * (DcId-1) + WorkerId rem WPerNode + 1,
    %lager:info("MyDc is ~w, warehouse is ~w", [DcId, WarehouseId]),
    DistrictId = tpcc_tool:random_num(1, ?NB_MAX_DISTRICT),
    CustomerId = tpcc_tool:non_uniform_random(C_C_ID, ?A_C_ID, 1, ?NB_MAX_CUSTOMER),
    NumItems = tpcc_tool:random_num(?MIN_ITEM, ?MAX_ITEM),
    %lager:info("DistrictId is ~w, Customer Id is ~w, NumItems is ~w", [DistrictId, CustomerId, NumItems]),

    %TxId = {tx_id, tpcc_tool:now_nsec(), self()}, %,gen_server:call({global, TxServer}, {start_tx}),
    TxId = gen_server:call({global, TxServer}, {start_tx}),
    %lager:info("TxId is ~w", [TxId]),
    CustomerKey = tpcc_tool:get_key_by_param({WarehouseId, DistrictId, CustomerId}, customer), 
    _Customer = read_from_node(TxServer, TxId, CustomerKey, to_dc(WarehouseId, WPerNode), DcId, PartList, HashDict), 
    WarehouseKey = tpcc_tool:get_key_by_param({WarehouseId}, warehouse),

    %% ************ read time *****************
    RT1 = os:timestamp(),
    %% ************ read time *****************

    _Warehouse = read_from_node(TxServer, TxId, WarehouseKey, to_dc(WarehouseId, WPerNode), DcId, PartList, HashDict),

    DistrictKey = tpcc_tool:get_key_by_param({WarehouseId, DistrictId}, district),
    District = read_from_node(TxServer, TxId, DistrictKey, to_dc(WarehouseId, WPerNode), DcId, PartList, HashDict),
    OId = District#district.d_next_o_id,

    NewOrder = tpcc_tool:create_neworder(WarehouseId, DistrictId, OId),
    NewOrderKey = tpcc_tool:get_key_by_param({WarehouseId, DistrictId, OId}, neworder),
    WS1 = dict:store({WarehouseId, NewOrderKey}, NewOrder, WS), 
    %LocalWS1 = add_to_writeset(NewOrderKey, NewOrder, lists:nth(DcId, PartList), LocalWS),
    District1 = District#district{d_next_o_id=(OId+1) rem ?MAX_NEW_ORDER},
    %LocalWS2 = add_to_writeset(DistrictKey, District1, lists:nth(DcId, PartList), LocalWS1),
    WS2 = dict:store({WarehouseId, DistrictKey}, District1, WS1), 

    Seq = lists:seq(1, NumItems),
    {WS3, _, AllLocal} = lists:foldl(fun(OlNumber, {TWS, TRS, AL}) ->
                    WId = pick_warehouse(DcId, MyRepIds, NoRepIds, WPerNode, AccessMaster, AccessSlave),
                    {Min, Max} = lists:nth(to_dc(WId, WPerNode), ItemRanges),
                    %ItemId = case tpcc_tool:random_num(1, 100) of
                    %            1 ->
                    %                -12345;
                    %            _ ->
                    ItemId = tpcc_tool:non_uniform_random(C_OL_I_ID, ?A_OL_I_ID, Min, Max),
                    %lager:info("MyDc is ~w, item warehouse id is ~w, Id is ~w", [DcId, WId, ItemId]),
                             %end,
                    Quantity = tpcc_tool:random_num(1, ?NB_MAX_DISTRICT),
                    ItemKey = tpcc_tool:get_key_by_param({ItemId}, item),
                    %Item = read_from_node(TxServer, TxId, ItemKey, WId, PartList, MyRepList),
                    {Item, TRS1} = read_from_cache_or_node(TRS, TxServer, TxId, ItemKey, to_dc(WId, WPerNode), DcId, PartList, HashDict),
                    StockKey = tpcc_tool:get_key_by_param({WId, ItemId}, stock),
                    %Stock = read_from_node(TxServer, TxId, StockKey, WId, PartList, MyRepList),
                    {Stock, TRS2} = read_from_cache_or_node(TRS1, TxServer, TxId, StockKey, to_dc(WId, WPerNode), DcId, PartList, HashDict),
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
    {LocalWriteList, RemoteWriteList} = get_local_remote_writeset(WS4, PartList, DcId, WPerNode, new_order, MyTable),
    %lager:info("Local Write set is ~p", [LocalWriteList]),
    %lager:info("Remote Write set is ~p", [RemoteWriteList]),
    %DepsList = ets:lookup(dep_table, TxId),
    %% ************ read time *****************
    RT2 = os:timestamp(),
    %% ************ read time *****************
    Response =  gen_server:call({global, TxServer}, {certify, TxId, LocalWriteList, RemoteWriteList}, ?TIMEOUT),%, length(DepsList)}),
    RT3 = os:timestamp(),
    case Response of
        {ok, {committed, _}} ->
            case RemoteWriteList of
                [] ->
                    {NOLTime, NOLCount} = NOLocal,
                    {ok, State#state{no_read=NORead+get_time_diff(RT1, RT2), no_local={NOLTime+get_time_diff(RT2, RT3), 
                            NOLCount+1}}};
                _ ->
                    {NORTime, NORCount} = NORemote,
                    {ok, State#state{no_read=NORead+get_time_diff(RT1, RT2), no_remote={NORTime+get_time_diff(RT2, RT3), 
                            NORCount+1}}}
            end;
        {ok, {specula_commit, _}} ->
            {NOSTime, NOSCount} = NOSpecula,
            {ok, State#state{no_read=NORead+get_time_diff(RT1, RT2), no_specula={NOSTime+get_time_diff(RT2, RT3), 
                    NOSCount+1}}};
        {error,timeout} ->
            lager:info("Timeout on client ~p",[TxServer]),
            {error, timeout, State};
        {aborted, local} ->
	    random:seed(os:timestamp()),
            {NOLTime, NOLCount} = NOLAbort,
            {error, aborted, State#state{no_read=NORead+get_time_diff(RT1, RT2), no_local_abort={NOLTime+get_time_diff(RT2, RT3), 
                    NOLCount+1}}};
        {aborted, remote} ->
	    random:seed(os:timestamp()),
            {NORTime, NORCount} = NORAbort,
            {error, aborted, State#state{no_read=NORead+get_time_diff(RT1, RT2), no_remote_abort={NORTime+get_time_diff(RT2, RT3), 
                    NORCount+1}}};
        {aborted, _} ->
	    random:seed(os:timestamp()),
            {error, aborted, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end;

%% @doc Payment transaction of TPC-C
run(payment, _KeyGen, _ValueGen, State=#state{part_list=PartList, tx_server=TxServer, worker_id=WorkerId,
        hash_dict=HashDict, my_rep_ids=MyRepIds, no_rep_ids=NoRepIds, w_per_dc=WPerNode, my_table=MyTable,
        node_id=DcId, access_slave=AccessSlave, payment_prep=PaymentPrep, payment_read=PaymentRead,
        c_c_id=C_C_ID, c_c_last = C_C_LAST, access_master=AccessMaster}) ->
    WS = dict:new(),
    %LocalWS = dict:new(),
    %RemoteWS = dict:new(),
    TWarehouseId = WPerNode * (DcId-1) + WorkerId rem WPerNode +1, 
	DistrictId = tpcc_tool:random_num(1, ?NB_MAX_DISTRICT),
	
    %% TODO: this should be changed. 
    %{CWId, CDId} = case tpcc_tool:random_num(1, 100) =< AccessMaster of
	%			        true ->
	%			            {TWarehouseId, DistrictId};
	%			        false ->
	%						RId = tpcc_tool:random_num(1, ?NB_MAX_DISTRICT),
	%			    		N = tpcc_tool:random_num(1, NumDcs-1),
	%						case N >= TWarehouseId of
	%							true -> {N+1, RId};  false -> {N, RId}
	%						end
	%			  	end,
    CWId = pick_warehouse(DcId, MyRepIds, NoRepIds, WPerNode, AccessMaster, AccessSlave),
    CDId = DistrictId,
	PaymentAmount = tpcc_tool:random_num(100, 500000) / 100.0,

    %% Only customer can be remote, everything else(Warehouse, District) should be local
	%TxId = {tx_id, tpcc_tool:now_nsec(), self()},	
    TxId = gen_server:call({global, TxServer}, {start_tx}),
	WarehouseKey = tpcc_tool:get_key_by_param({TWarehouseId}, warehouse),
    %% ************ read time *****************
    RT1 = os:timestamp(),
    %% ************ read time *****************
    Warehouse = read_from_node(TxServer, TxId, WarehouseKey, to_dc(TWarehouseId, WPerNode), DcId, PartList, HashDict),
	WYtdKey = WarehouseKey++":w_ytd",
	WYtd = read_from_node(TxServer, TxId, WYtdKey, to_dc(TWarehouseId, WPerNode), DcId, PartList, HashDict),
	WYtd1 = WYtd+ PaymentAmount,
	WS1 = dict:store({TWarehouseId, WYtdKey}, WYtd1, WS),
	DistrictKey = tpcc_tool:get_key_by_param({TWarehouseId, DistrictId}, district),
    District = read_from_node(TxServer, TxId, DistrictKey, to_dc(TWarehouseId, WPerNode), DcId, PartList, HashDict),
	DYtdKey = DistrictKey++":d_ytd",
	DYtd = read_from_node(TxServer, TxId, DYtdKey, to_dc(TWarehouseId, WPerNode), DcId, PartList, HashDict),
	DYtd1 = DYtd+ PaymentAmount,
	WS2 = dict:store({TWarehouseId, DYtdKey}, DYtd1, WS1),
	
		%% 60% change to load customer by name, otherwise not by name
    CW = case tpcc_tool:random_num(1, 100) =< 60 of
			true ->
				Rand = trunc(tpcc_tool:non_uniform_random(C_C_LAST, ?A_C_LAST, 0, ?MAX_C_LAST)),
	         	CLastName = tpcc_tool:last_name(Rand),
				CustomerLookupKey = tpcc_tool:get_key_by_param({CWId, CDId, CLastName}, customer_lookup),
				CustomerLookup = read_from_node(TxServer, TxId, CustomerLookupKey, to_dc(CWId, WPerNode), DcId, PartList, HashDict),
                Ids = CustomerLookup#customer_lookup.ids,
                Customers= lists:foldl(fun(Id, Acc) ->
                            CKey = tpcc_tool:get_key_by_param({CWId, CDId, Id}, customer),
                            C = read_from_node(TxServer, TxId, CKey, to_dc(CWId, WPerNode), DcId, PartList, HashDict),
                            case C of
                                error -> Acc;  _ -> [C|Acc]
                            end end, [], Ids),
                SortedCustomers = lists:sort(Customers),
                Middle = (length(Customers) + 1) div 2,
                lists:nth(Middle, SortedCustomers);
	       	false ->
	         	CustomerID = tpcc_tool:non_uniform_random(C_C_ID, ?A_C_ID, 1, ?NB_MAX_CUSTOMER),
				CKey = tpcc_tool:get_key_by_param({CWId, CDId, CustomerID}, customer),
				read_from_node(TxServer, TxId, CKey, to_dc(CWId, WPerNode), DcId, PartList, HashDict)
		end,
    case CW of
        error ->
            {error, not_found, State};
        _ ->
            CWBalanceKey = tpcc_tool:get_key(CW)++":c_balance",
            CWBalance = read_from_node(TxServer, TxId, CWBalanceKey, to_dc(CWId, WPerNode), DcId, PartList, HashDict),
	    %% ************ read time *****************
	    RT2 = os:timestamp(),
	    %% ************ read time *****************
            CWBalance1 = CWBalance + PaymentAmount,
            WS3 = dict:store({CWId, CWBalanceKey}, CWBalance1, WS2),
            WName = Warehouse#warehouse.w_name,
            DName = District#district.d_name,
            HData = lists:sublist(WName, 1, 10) ++ "  " ++ lists:sublist(DName, 1, 10),
            %% History should be local
            History = tpcc_tool:create_history(TWarehouseId, DistrictId, CWId, CDId, 
                                               CW#customer.c_id, tpcc_tool:now_nsec(), PaymentAmount, HData),
            HistoryKey = tpcc_tool:get_key(History),
            WS4 = dict:store({TWarehouseId, HistoryKey}, History, WS3),
            {LocalWriteList, RemoteWriteList} = get_local_remote_writeset(WS4, PartList, DcId, WPerNode, payment, MyTable),
            %DepsList = ets:lookup(dep_table, TxId),
            T1 = os:timestamp(),
            Response =  gen_server:call({global, TxServer}, {certify, TxId, LocalWriteList, RemoteWriteList}, ?TIMEOUT),%, length(DepsList)}),
            T2 = os:timestamp(),
	    {AccT, AccN} = PaymentPrep,
	    {AccRT, AccN} = PaymentRead, 
            case Response of
                {ok, _Value} ->
                    {ok, State#state{payment_prep={get_time_diff(T1, T2)+AccT, AccN+1},
				payment_read={get_time_diff(RT1, RT2)+AccRT, AccN+1}}};
                {error,timeout} ->
                    lager:info("Timeout on client ~p",[TxServer]),
                    {error, timeout, State};
                {aborted, _} ->
	    	    random:seed(os:timestamp()),
                    %lager:error("Aborted"),
                    {error, aborted, State};
                {badrpc, Reason} ->
                    {error, Reason, State}
            end
    end;

%% @doc Payment transaction of TPC-C
run(order_status, _KeyGen, _ValueGen, State=#state{part_list=PartList, tx_server=TxServer,
        hash_dict=HashDict, node_id=DcId, w_per_dc=WPerNode, worker_id=WorkerId, 
        c_c_id=C_C_ID, c_c_last = C_C_LAST}) ->
    TWarehouseId = WPerNode * (DcId-1) + WorkerId rem WPerNode +1, 
	DistrictId = tpcc_tool:random_num(1, ?NB_MAX_DISTRICT),
	
	%TxId = {tx_id, tpcc_tool:now_nsec(), self()},
    TxId = gen_server:call({global, TxServer}, {start_tx}),
	CW = case tpcc_tool:random_num(1, 100) =< 60 of
			true ->
				Rand = trunc(tpcc_tool:non_uniform_random(C_C_LAST, ?A_C_LAST, 0, ?MAX_C_LAST)),
	         	CLastName = tpcc_tool:last_name(Rand),
				CustomerLookupKey = tpcc_tool:get_key_by_param({TWarehouseId, DistrictId, CLastName}, customer_lookup),
				CustomerLookup = read_from_node(TxServer, TxId, CustomerLookupKey, to_dc(TWarehouseId, WPerNode), DcId, PartList, HashDict),
                case CustomerLookup of
                    error ->
                        lager:error("Key not found by last name ~p", [CLastName]),
                        error;
                    _ ->
                        Ids = CustomerLookup#customer_lookup.ids,
                        Customers= lists:foldl(fun(Id, Acc) ->
                                    CKey = tpcc_tool:get_key_by_param({TWarehouseId, DistrictId, Id}, customer),
                                    C = read_from_node(TxServer, TxId, CKey, to_dc(TWarehouseId, WPerNode), DcId, PartList, HashDict),
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
				read_from_node(TxServer, TxId, CKey, to_dc(TWarehouseId, WPerNode), DcId, PartList, HashDict)
		end,
    CWLastOrder = CW#customer.c_last_order,
    OrdKey = tpcc_tool:get_key_by_param({TWarehouseId, DistrictId, CWLastOrder}, order),
    LastOne = read_from_node(TxServer, TxId, OrdKey, to_dc(TWarehouseId, WPerNode), DcId, PartList, HashDict),
    %lager:info("CWId is ~w, length of orderlist is ~w", [CWId, length(OrderList)]),
    NumLines = LastOne#order.o_ol_cnt,
    Seq2 = lists:seq(1, NumLines),
    %lager:info("Loading ~w orderlines", [NumLines]),
    OWId = LastOne#order.o_w_id,
    ODId = LastOne#order.o_d_id,
    OId = LastOne#order.o_id,
    lists:foreach(fun(Number) ->
            OlKey = tpcc_tool:get_key_by_param({OWId, ODId, OId, Number}, orderline),
            _Ol = read_from_node(TxServer, TxId, OlKey, to_dc(TWarehouseId, WPerNode), DcId, PartList, HashDict)
            end, Seq2),
    _ =  gen_server:call({global, TxServer}, {certify, TxId, [], []}, ?TIMEOUT),
    {ok, State}.

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

read_from_node(TxServer, TxId, Key, DcId, MyDcId, PartList, HashDict) ->
    {ok, V} = case DcId of
        MyDcId ->
            {_, L} = lists:nth(DcId, PartList),
            Index = crypto:bytes_to_integer(erlang:md5(Key)) rem length(L) + 1,
            Part = lists:nth(Index, L),
            tx_cert_sup:read(TxServer, TxId, Key, Part);
        _ ->
            case dict:find(DcId, HashDict) of
                error ->
                    {_, L} = lists:nth(DcId, PartList),
                    Index = crypto:bytes_to_integer(erlang:md5(Key)) rem length(L) + 1,
                    Part = lists:nth(Index, L),
                    {CacheServName, _} = lists:nth(MyDcId, PartList),
                    cache_serv:read(CacheServName, Key, TxId, Part);
                {ok, N} ->
                    {_, L} = lists:nth(DcId, PartList),
                    Index = crypto:bytes_to_integer(erlang:md5(Key)) rem length(L) + 1,
                    Part = lists:nth(Index, L),
                    data_repl_serv:read(N, Key, TxId, Part)
            end
    end,
    case V of
        [] ->
            lager:error("Key ~p not found!!!! Should read from dc ~w, my dc is ~w", [Key, DcId, MyDcId]),
            error;
        _ ->
            V
    end.
    %case Res of
    %    {specula, DepTx} ->
    %        ets:insert(dep_table, {TxId, DepTx});
    %    ok ->
    %        ok
    %end,

read_from_cache_or_node(ReadSet, TxServer, TxId, Key, DcId, MyDcId, PartList, HashDict) ->
    case dict:find(Key, ReadSet) of
        {ok, V} ->
            %lager:info("In read set..Key ~p, V ~p, Readset ~p", [Key, V, ReadSet]),
            {V, ReadSet};
        error ->
            V = read_from_node(TxServer, TxId, Key, DcId, MyDcId, PartList, HashDict),
            ReadSet1 = dict:store(Key, V, ReadSet),
            %lager:info("Not in read set..Key ~p, V ~p, Readset ~p", [Key, V, ReadSet1]),
            {V, ReadSet1}
    end.
 
read(TxServer, TxId, Key, ExpandPartList, HashLength) ->
    Part = get_partition(Key, ExpandPartList, HashLength),
    {ok, V} = tx_cert_sup:read(TxServer, TxId, Key, Part),
    case V of
        [] ->
            lager:error("Key ~p not found!!!!", [Key]),
            error;
        _ ->
            %lager:info("Reading ~p, ~p", [Key, V]),
            V
    end.

terminate(_, _State=#state{no_local=NOLocal, no_remote=NORemote, no_local_abort=NOLAbort, no_remote_abort=NORAbort, 
            no_specula=NOSpecula, no_read=NORead, payment_prep=PaymentPrep, payment_read=PaymentRead, my_table=MyTable}) ->
    {T1, C1} = NOLocal,
    {T2, C2} = NORemote,
    {T3, C3} = NOLAbort,
    {T4, C4} = NORAbort,
    {T5, C5} = NOSpecula,
    LCT = T1 div max(1, C1),
    RCT = T2 div max(1, C2),
    LA = T3 div max(1, C3),
    RA = T4 div max(1, C4),
    SCT = T5 div max(1, C5),
    NOR = NORead div max(1, C1+C2+C3+C4+C5),
    {_PPrep, _PRead} = case PaymentPrep of
			{0, 0} -> {0, 0};
			{AccT1, AccN1} -> {AccRT1, AccN1} = PaymentRead,
                                        {AccT1 div AccN1, AccRT1 div AccN1} 
		     end,
    File= "prep",
    [{payment, PT, PP, PN}] = ets:lookup(MyTable, payment),
    [{new_order, NT, NP, NN}] = ets:lookup(MyTable, new_order),
    %lager:info("File is ~p, Value is ~p, ~p, ~p, ~p, ~p, ~p, ~p", [File, NOPrep, NORead1, NORead2, NORead3, NOItem, PPrep, PRead]),
    file:write_file(File, io_lib:fwrite("~p ~p ~p  ~p  ~p ~p ~p ~p ~p ~p\n", 
            [NOR, LA,  RA, LCT, RCT, SCT, PP/PT ,PN/PT, NP/NT, NN/NT]), [append]).

get_local_remote_writeset(WriteSet, PartList, LocalDcId, WPerNode, TxType, MyTable) ->
    {LWSD, RWSD} = dict:fold(fun({WId, Key}, Value, {LWS, RWS}) ->
                    Id = (WId-1) div WPerNode + 1, 
                    case Id of LocalDcId -> {add_to_writeset(Key, Value, lists:nth(LocalDcId, PartList), LWS), RWS};
                               _ -> {LWS, add_to_writeset(Key, Value, lists:nth(Id, PartList), RWS)}
                    end end, {dict:new(), dict:new()}, WriteSet),
    L = dict:fetch_keys(RWSD),
    NodeSet = lists:foldl(fun({_, N}, S) -> sets:add_element(N, S) end, sets:new(), L),
    ets:update_counter(MyTable, TxType, [{2, 1}, {3, length(L)}, {4,sets:size(NodeSet)}]),
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
    %lager:info("Trying to get index: PL ~w, List ~w", [PL, List]),
    [index(X, List) || X <- PL ].

pick_warehouse(MyId, RepIds, NoRepIds, WPerNode, AccessMaster, AccessRep) ->
    %lager:info("~w ~w ~w ~w ~w ~w", [MyId, RepIds, NoRepIds, WPerNode, AccessMaster, AccessRep]),
    R = random:uniform(100),
    case R =< AccessMaster of
        true ->
            WPerNode*(MyId-1) + R rem WPerNode +1;
        false ->
            case R =< AccessMaster + AccessRep of
                true ->
                    L = length(RepIds),
                    N = R rem (L * WPerNode),
                    F = N div WPerNode +1, 
                    S = N rem WPerNode,
                    WPerNode*(lists:nth(F, RepIds)-1)+S+1;
                false ->
                    L = length(NoRepIds),
                    case L of 0 ->
                                WPerNode*(MyId-1) + R rem WPerNode +1;
                            _ ->
                                N = R rem (L * WPerNode) + 1,
                                F = (N-1) div WPerNode +1, 
                                S = N rem WPerNode,
                                WPerNode*(lists:nth(F, NoRepIds)-1)+S+1
                    end
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

%get_replica(_, []) ->
%    false;
%get_replica(E, [{E, N}|_]) ->
%    N;
%get_replica(E, [_|L]) ->
%    get_replica(E, L).

get_rep_name(Target, Rep) ->
    list_to_atom(atom_to_list(Target)++"repl"++atom_to_list(Rep)).

    
get_time_diff({A1, B1, C1}, {A2, B2, C2}) ->
    ((A2-A1)*1000000+ (B2-B1))*1000000+ C2-C1.

to_dc(WId, WPerNode) ->
    (WId-1) div WPerNode + 1.

build_local_norep_dict(NodeId, AllNodes, NoRepIds, NumDcs) ->
    case length(AllNodes) of NumDcs -> dict:new();
		_ -> 
		    NodesPerDc = length(AllNodes) div NumDcs,
		    DcId = (NodeId-1) div NodesPerDc +1,
		    lists:foldl(fun(NoRepId, Dict) ->
			    Rem = NoRepId rem NodesPerDc + 1,
			    RepedByLocal = Rem + (DcId-1) * NodesPerDc,
			    RepedLocalNode = lists:nth(RepedByLocal, AllNodes),
			    NoRepNode = lists:nth(NoRepId, AllNodes),
			    lager:info("nEWdICT IS ~w", [Dict]),
			    dict:store(NoRepId, get_rep_name(RepedLocalNode, NoRepNode), Dict)
			    end, dict:new(), NoRepIds)
    end.
            
                    
                    
    
        



