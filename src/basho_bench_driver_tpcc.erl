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

    M = [L || {N, L} <- ReplList, N == TargetNode ],
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

    ExpandPartList = lists:flatten([L || {_, L} <- PartList]),
    lager:info("Ex list is ~w", [ExpandPartList]),
    HashLength = length(ExpandPartList),

    lager:info("Part list is ~w",[PartList]),
    timer:sleep(1000),
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
    LocalWS = dict:new(),
    RemoteWS = dict:new(),
    DistrictId = tpcc_tool:random_num(1, ?NB_MAX_DISTRICT),
    CustomerId = tpcc_tool:non_uniform_random(C_C_ID, ?A_C_ID, 1, ?NB_MAX_CUSTOMER),
    NumItems = tpcc_tool:random_number(?MIN_ITEM, ?MAX_ITEM),
    lager:info("DistrictId is ~w, Customer Id is ~w, NumItems is ~w", [DistrictId, CustomerId, NumItems]),

    TxId = tx_utilities:create_transaction_record(0),
    lager:info("TxId is ~w", [TxId]),
    CustomerKey = tpcc_tool:get_key_by_param({DcId, DistrictId, CustomerId}, customer), 
    _Customer = read_from_node(TxServer, TxId, CustomerKey, DcId, PartList, MyRepList), 
    WarehouseKey = tpcc_tool:get_key_by_param({DcId}, warehouse),
    _Warehouse = read_from_node(TxServer, TxId, WarehouseKey, DcId, PartList, MyRepList),

    DistrictKey = tpcc_tool:get_key_by_param({DcId, DistrictId}, district),
    District = read_from_node(TxServer, TxId, DistrictKey, DcId, PartList, MyRepList),
    OId = District#district.d_next_o_id,

    NewOrder = tpcc_tool:create_neworder(DcId, DistrictId, OId),
    NewOrderKey = tpcc_tool:get_key_by_param({DcId, DistrictId, OId}, neworder),
    LocalWS1 = add_to_writeset(NewOrderKey, NewOrder, lists:nth(DcId, PartList), LocalWS),
    District1 = District#district{d_next_o_id=OId+1},
    LocalWS2 = add_to_writeset(DistrictKey, District1, lists:nth(DcId, PartList), LocalWS1),

    Seq = lists:seq(1, NumItems),
    {LocalWS3, RemoteWS3, AllLocal} = lists:foldl(fun(OlNumber, {LWS, RWS, AL}) ->
                    WId = pick_warehouse(DcId, MyRepIds, NoRepIds, AccessMaster, AccessSlave),
                    {Min, Max} = lists:nth(WId, ItemRanges),
                    ItemId = case tpcc_tool:random_uniform(1, 100) of
                                1 ->
                                    -12345;
                                _ ->
                                    tpcc_tool:non_uniform_random(C_OL_I_ID, ?A_OL_I_ID, Min, Max)
                             end,
                    Quantity = tpcc_tools:random_number(1, ?NB_MAX_DISTRICT),
                    ItemKey = tpcc_tool:get_key_by_param({ItemId}, item),
                    Item = read_from_node(TxServer, TxId, ItemKey, WId, PartList, MyRepList),
                    StockKey = tpcc_tool:get_key_by_param({WId, ItemId}, stock),
                    Stock = read_from_node(TxServer, TxId, StockKey, WId, PartList, MyRepList),
                    NewSQuantity = case Stock#stock.s_quantity - Quantity >= 10 of
                                        true -> s_quantity - Quantity;
                                        false -> s_quantity - Quantity + 91
                                    end,
                    SRemote = case WId of 
                                    DcId -> Stock#stock.s_remote_cnt;
                                    _ -> Stock#stock.s_remote_cnt+1
                                end,
                    SYtd = Stock#stock.s_ytd,
                    SOrderCnt = Stock#stock.s_order_cnt,
                    Stock1 = Stock#stock{s_quantity=NewSQuantity, s_ytd=SYtd+Quantity, s_remote_cnt=SRemote,
                                   s_order_cnt=SOrderCnt+1},                    
                    {LWS1, RWS1} = case WId of
                                        DcId -> {add_to_writeset(StockKey, Stock1, lists:nth(DcId, PartList), LWS), RWS};
                                        _ -> {LWS, add_to_writeset(StockKey, Stock1, lists:nth(WId, PartList), RWS)}
                                    end,
                    OlAmount = Quantity * Item#item.i_price,
                    %IData = Item#item.i_data,
                    %SData = Stock#stock.s_data,;
                    OlDistInfo = get_district_info(Stock1, DistrictId),
                    Orderline = tpcc_tool:create_orderline(DcId, DistrictId, WId, OId, ItemId, 
                        OlNumber, Quantity, OlAmount, OlDistInfo),
                    LWS2 = add_to_writeset(tpcc_tool:get_key(Orderline), Orderline, lists:nth(DcId, PartList), LWS1),
                    AL1 = case WId of 
                                    DcId -> AL;
                                    _ -> 0 
                                end,
                    {LWS2, RWS1, AL1}
            end, {LocalWS2, RemoteWS, 1}, Seq),
    Order = tpcc_tool:create_order(DcId, DistrictId, OId, NumItems, CustomerId, tpcc_tool:now_nsec(), AllLocal), 
    OrderKey = tpcc_tool:get_key_by_param({DcId, DistrictId, OId}, order),
    LocalWS4 = add_to_writeset(OrderKey, Order, lists:nth(DcId, PartList), LocalWS3),
    lager:info("FinalWrite set is ~p, ~p", [LocalWS4, RemoteWS3]),
    Response =  gen_server:call({global, TxServer}, {certify, TxId, dict:to_list(LocalWS4), dict:to_list(RemoteWS3)}),
    case Response of
        {ok, _Value} ->
            {ok, State};
        {error,timeout} ->
            lager:info("Timeout on client ~p",[TxServer]),
            {error, timeout, State};
        {aborted, _} ->
            lager:error("Aborted"),
            {error, aborted, State};
        {badrpc, Reason} ->
            {error, Reason, State}
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
                        tx_cert_sup:read(TxServer, TxId, Key, Part);
                    N ->
                        data_repl_server:read(N, TxId, Key)
                end,
    case V of
        [] ->
            lager:error("Key not found!!!!");
        _ ->
            V
    end.
 
read(TxServer, TxId, Key, ExpandPartList, HashLength) ->
    Part = get_partition(Key, ExpandPartList, HashLength),
    {ok, V} = tx_cert_sup:read(TxServer, TxId, Key, Part),
    case V of
        [] ->
            lager:error("Key not found!!!!");
        _ ->
            lager:info("Reading ~p, ~p", [Key, V]),
            V
    end.

add_to_writeset(Key, Value, PartList, WSet) ->
    Index = crypto:bytes_to_integer(erlang:md5(Key)) rem length(PartList) + 1,
    Part = lists:nth(Index, PartList),
    lager:info("Adding  ~p, ~p to ~w", [Key, Value, Part]),
    dict:append(Part, {Key, Value}, WSet).

get_indexes(PL, List) ->
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
                    Acc++[{FirstItem, LastItem}] 
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

    
