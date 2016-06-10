% -------------------------------------------------------------------
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
-module(basho_bench_driver_rubis).

-export([new/1,
	    read/5,
        terminate/2,
        run/4]).

-include("basho_bench.hrl").
-include("rubis.hrl").

-define(TIMEOUT, 10000).
-define(READ_TIMEOUT, 15000).

-record(prev_state, {last_user_id = undef :: {integer(), integer()},
                     item_id = undef :: {integer(), integer()},
                     region = undef :: {integer(), integer()},
                     myself_id = undef :: {integer(), integer()},
                     min_bid = 1000000 :: integer(),
                     max_qty = -1 :: integer()}).

-record(state, {worker_id,
                request,
                part_list,
                expand_part_list,
                hash_length,
                my_table,
                to_sleep,
                hash_dict,
                other_master_ids,
                dc_rep_ids,
                no_rep_ids,
                prev_state,
                num_nodes,
                num_replicates,
                access_master,
                access_slave,
                %% Rubis state
                nb_users,
                nb_old_items,
                nb_regions,
                nb_categories,
                max_quantity,
                percent_buy_now,
                percent_reserve_item,
                percent_unique_item,
                max_duration,
                
                node_id,
                specula,
                tx_server,
                rubis_state,
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
    Specula = basho_bench_config:get(specula),
   
    AccessMaster = basho_bench_config:get(access_master),
    AccessSlave = basho_bench_config:get(access_slave),

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
    %lager:info("NumDcs is ~w", [NumDcs]),
    %lager:info("Part list is ~w, repl list is ~w", [PartList, ReplList]),

    %lager:info("My Rep Ids is ~p, my rep list is ~p", [MyRepIds, MyRepList]),
    AllNodes = [N || {N, _} <- PartList],
    [NumReplicates] = [length(L) || {N, L} <- ReplList, N == TargetNode],
    NodeId = index(TargetNode, AllNodes),
    NumNodes = length(AllNodes),
    case Id of 1 -> timer:sleep(MasterToSleep);
    	       _ -> timer:sleep(ToSleep)
    end,
    MyTxServer = locality_fun:get_pid(TargetNode, list_to_atom(atom_to_list(TargetNode) 
            ++ "-cert-" ++ integer_to_list((Id-1) div length(IPs)+1))),

    {OtherMasterIds, DcRepIds, DcNoRepIds, HashDict} = locality_fun:get_locality_list(PartList, ReplList, NumDcs, TargetNode, single_dc_read),
    HashDict1 = locality_fun:replace_name_by_pid(TargetNode, dict:store(cache, TargetNode, HashDict)),
    %lager:info("OtherMasterId is ~w, DcRep Id is ~w", [OtherMasterIds, DcRepIds]),

    ExpandPartList = lists:flatten([L || {_, L} <- PartList]),
    HashLength = length(ExpandPartList),

    ConfigDict = load_config(),
    NBUsers = dict:fetch(database_number_of_users, ConfigDict) div dict:fetch(reduce_factor, ConfigDict), 
    NBOldItems = dict:fetch(database_number_of_old_items, ConfigDict) div dict:fetch(reduce_factor, ConfigDict), 
    NBRegions = dict:fetch(nb_regions, ConfigDict), 
    NBCategories = dict:fetch(nb_categories, ConfigDict), 
    MaxQuantity = dict:fetch(database_max_quantity_for_multiple_items, ConfigDict) div dict:fetch(reduce_factor, ConfigDict), 
    PercentBuyNow = dict:fetch(database_percentage_of_buy_now_items, ConfigDict) div dict:fetch(reduce_factor, ConfigDict), 
    PercentResvItem = dict:fetch(database_percentage_of_items_with_reserve_price, ConfigDict) div dict:fetch(reduce_factor, ConfigDict), 
    PercentUniqueItem = dict:fetch(database_percentage_of_unique_items, ConfigDict) div dict:fetch(reduce_factor, ConfigDict), 
    MaxDuration = dict:fetch(max_duration, ConfigDict) div dict:fetch(reduce_factor, ConfigDict), 
    TabName = list_to_atom(pid_to_list(MyTxServer)),
    ets:new(TabName, [set, named_table, {read_concurrency,false}]),
    ets:insert(TabName, {master, 0}),
    ets:insert(TabName, {slave, 0}),
    ets:insert(TabName, {remote, 0}),
    {ok, #state{worker_id=Id,
               tx_server=MyTxServer,
               access_master=AccessMaster,
               access_slave=AccessSlave,
               part_list = PartList,
               hash_dict = HashDict1,
               prev_state=#prev_state{myself_id= {NodeId, random:uniform(NBUsers)}},
               num_replicates = NumReplicates,
               nb_users = NBUsers,
               nb_old_items = NBOldItems,
               nb_regions = NBRegions,
               nb_categories = NBCategories,
               max_quantity = MaxQuantity,
               percent_buy_now = PercentBuyNow,
               percent_reserve_item = PercentResvItem,
               percent_unique_item = PercentUniqueItem,
               max_duration = MaxDuration,
               other_master_ids = OtherMasterIds,
               dc_rep_ids = DcRepIds,
               no_rep_ids = DcNoRepIds,
               %no_rep_list = SlaveRepList,
               to_sleep=ToSleep,
               expand_part_list = ExpandPartList,
               hash_length = HashLength,   
               specula = Specula,
               num_nodes = NumNodes,
               node_id = NodeId,
               target_node=TargetNode}}.

terminate(_, _State=#state{tx_server=TxServer}) ->
    TabName = list_to_atom(pid_to_list(TxServer)),
    [{_, MasterRead}]= ets:lookup(TabName, master),
    [{_, SlaveRead}]= ets:lookup(TabName, slave),
    [{_, RemoteRead}]= ets:lookup(TabName, remote),
    File="prep",
    file:write_file(File, io_lib:fwrite(" ~p ~p ~p ~p\n",
            [MasterRead, SlaveRead, RemoteRead, RemoteRead/(MasterRead+SlaveRead+RemoteRead)]), [append]).


%% VERIFIED
run(home, _KeyGen, _ValueGen, State=#state{specula=Specula, tx_server=TxServer, prev_state=PrevState,
            part_list=PartList, hash_dict=HashDict, node_id=MyNode, nb_users=NBUsers}) ->
    TxId = gen_server:call(TxServer, {start_tx}),
    PrevState1 = PrevState#prev_state{myself_id= {MyNode, random:uniform(NBUsers)}},
    MyselfId = PrevState#prev_state.myself_id,
    MyselfKey = rubis_tool:get_key(MyselfId, user),
    Myself = read_from_node(TxServer, TxId, MyselfKey, MyNode, MyNode, PartList, HashDict),
    %lager:info("Myself is ~w", [Myself]),
    Region = Myself#user.u_region,
    
    case Specula of
        true ->
            _ =  gen_server:call(TxServer, {certify, TxId, [], []}, ?TIMEOUT);
        _ ->
            ok
    end,
    {ok, State#state{prev_state=PrevState1#prev_state{region={MyNode, Region}}}};

%% VERIFIED
run(register, _KeyGen, _ValueGen, State) ->
    {ok, State};

%% VERIFIED
%% Register a user that is replicated??
run(register_user, _KeyGen, _ValueGen, State=#state{nb_users=NBUsers, node_id=MyNode, tx_server=TxServer, 
            specula=Specula, part_list=PartList, hash_dict=HashDict, nb_regions=NBRegions,
            access_master=AccessMaster, access_slave=AccessSlave, dc_rep_ids=DcRepList, no_rep_ids=DcNoRepList}) ->
    UserId = random:uniform(NBUsers) + NBUsers,
    FirstName = "Great" ++ [UserId],
    LastName = "User" ++ [UserId],
    NickName = "user" ++ [UserId],
    Email = FirstName ++ "." ++ LastName ++ "@rubis.com",
    Password = "password" ++ [UserId],
    RegionId = UserId rem NBRegions + 1,
    %% Select region
    
    ToRegisterNode = pick_node(MyNode, DcRepList, DcNoRepList, AccessMaster, AccessSlave), 

    %lager:info("Trying to register user ~w !!!!", [UserId]),
    TxId = gen_server:call(TxServer, {start_tx, true, true}),
    Now = rubis_tool:now_nsec(),

    UserKey = rubis_tool:get_key({ToRegisterNode, UserId}, user),
    User = empty_read_from_node(TxServer, TxId, UserKey, ToRegisterNode, MyNode, PartList, HashDict),
    case User of
        [] -> %% Okay
            NewUser = rubis_tool:create_user(FirstName, LastName, NickName, Password, Email, Now, 0, 0, RegionId),
            WS1 = dict:store({ToRegisterNode, UserKey}, NewUser, dict:new()), 
            {LocalWriteList, RemoteWriteList} = get_local_remote_writeset(WS1, PartList, MyNode),
            Response =  gen_server:call(TxServer, {certify, TxId, LocalWriteList, RemoteWriteList}, ?TIMEOUT),%, length(DepsList)}),
            case Response of
                {ok, {committed, _}} ->
                    {ok, State}; %{prev_state=PrevState#prev_state{last_user_id={ToRegisterNode, UserId}}}};
                {ok, {specula_commit, _}} ->
                    {ok, State}; %#state{prev_state=PrevState#prev_state{last_user_id={ToRegisterNode, UserId}}}};
                {aborted, _} ->
                    {error, aborted, State} %#state{prev_state=PrevState#prev_state{last_user_id={ToRegisterNode, UserId}}}}
            end;
        _ -> 
            case Specula of
                true ->
                    _ =  gen_server:call(TxServer, {certify, TxId, [], []}, ?TIMEOUT);
                _ ->
                    ok
            end,
            {error, aborted, State} %#state{prev_state=PrevState#prev_state{last_user_id={ToRegisterNode, UserId}}}}
    end;

%% VERIFIED
run(browse, _KeyGen, _ValueGen, State) ->
    {ok, State};

%% VERIFIED
run(browse_categories, _KeyGen, _ValueGen, State=#state{tx_server=TxServer, nb_categories=NBCategories, 
            hash_dict=HashDict, part_list=PartList, node_id=MyNode, specula=Specula}) ->
    Seq = lists:seq(1, NBCategories),
    TxId = gen_server:call(TxServer, {start_tx}),
    lists:foreach(fun(N) ->
                CategoryKey = rubis_tool:get_key(N, category),
                _ = read_from_node(TxServer, TxId, CategoryKey, MyNode, MyNode, PartList, HashDict)
                end, Seq),
    case Specula of
        true ->
            _ =  gen_server:call(TxServer, {certify, TxId, [], []}, ?TIMEOUT);
        _ ->
            ok
    end,
    {ok, State};

%% VERIFIED
%% READ_SPECULA
run(search_items_in_category, _KeyGen, _ValueGen, State=#state{nb_categories=NBCategories, node_id=MyNode, tx_server=TxServer, 
            hash_dict=HashDict, prev_state=PrevState, part_list=PartList, specula=Specula, dc_rep_ids=DcRepIds,
            no_rep_ids=DcNoRepIds, access_master=AccessMaster, access_slave=AccessSlave}) ->
    CategoryNode = pick_node(MyNode, DcRepIds, DcNoRepIds, AccessMaster, AccessSlave),
    CategoryId = random:uniform(NBCategories),
    CategoryNewItemKey = rubis_tool:get_key({CategoryNode, CategoryId}, categorynewitems), 
    TxId = gen_server:call(TxServer, {start_tx, true, true}),
    CategoryNewItems = read_from_node(TxServer, TxId, CategoryNewItemKey, CategoryNode, MyNode, PartList, HashDict),
    case Specula of
        true ->
            _ =  gen_server:call(TxServer, {certify, TxId, [], []}, ?TIMEOUT);
        _ ->
            ok
    end,
    case CategoryNewItems of
        empty ->
            %lager:error("Key is ~w, Category new items are ~w", [CategoryNewItemKey, CategoryNewItems]);
            {ok, State};
        _ ->
            %lager:info("Category is ~w, CategoryNewItems are ~w", [CategoryId, CategoryNewItems]),
            RandomItemId = random(CategoryNewItems),
            {ok, State#state{prev_state=PrevState#prev_state{item_id=RandomItemId}}}
    end;

%% VERIFIED
run(browse_regions, _KeyGen, _ValueGen, State=#state{nb_regions=NBRegions, tx_server=TxServer, 
            hash_dict=HashDict, node_id=MyNode, part_list=PartList, specula=Specula}) ->
    Seq = lists:seq(1, NBRegions),
    TxId = gen_server:call(TxServer, {start_tx}),
    lists:foreach(fun(N) ->
                RegionKey = rubis_tool:get_key(N, region),
                _ = read_from_node(TxServer, TxId, RegionKey, MyNode, MyNode, PartList, HashDict)
                end, Seq),
    case Specula of
        true ->
            _ =  gen_server:call(TxServer, {certify, TxId, [], []}, ?TIMEOUT);
        _ ->
            ok
    end,
    {ok, State};

%% KINDA VERIFIED
run(browse_categories_in_region, _KeyGen, _ValueGen, State=#state{nb_categories=NBCategories, hash_dict=HashDict, 
            node_id=MyNode, part_list=PartList, tx_server=TxServer, specula=Specula}) ->
    Seq = lists:seq(1, NBCategories),
    TxId = gen_server:call(TxServer, {start_tx}),
    lists:foreach(fun(N) ->
                CategoryKey = rubis_tool:get_key(N, category),
                _ = read_from_node(TxServer, TxId, CategoryKey, MyNode, MyNode, PartList, HashDict)
                end, Seq),
    case Specula of
        true ->
            _ =  gen_server:call(TxServer, {certify, TxId, [], []}, ?TIMEOUT);
        _ ->
            ok
    end,
    {ok, State};

%% KINDA VERIFIED
%% READ_SPECULA
run(search_items_in_region, _KeyGen, _ValueGen, State=#state{node_id=MyNode, nb_regions=NBRegions, 
            tx_server=TxServer, prev_state=PrevState, hash_dict=HashDict, part_list=PartList, specula=Specula,
            access_master=AccessMaster, access_slave=AccessSlave, dc_rep_ids=DcRepIds, no_rep_ids=DcNoRepIds}) ->
    RegionNode = pick_node(MyNode, DcRepIds, DcNoRepIds, AccessMaster, AccessSlave),

    RegionId = random:uniform(NBRegions),
    %lager:info("Before reading region new items in search"),
    RegionNewItemKey = rubis_tool:get_key({RegionNode, RegionId}, regionnewitems), 
    %lager:info("After reading region new items in search"),
    TxId = gen_server:call(TxServer, {start_tx, true, true}),
    RegionNewItems = read_from_node(TxServer, TxId, RegionNewItemKey, RegionNode, MyNode, PartList, HashDict),
    case Specula of
        true ->
            _ =  gen_server:call(TxServer, {certify, TxId, [], []}, ?TIMEOUT);
        _ ->
            ok
    end,
    case RegionNewItems of
        empty ->
            {ok, State};
        _ ->
            %lager:info("Region is ~w, RegionNewItems are ~w", [RegionId, RegionNewItems]),
            RandomItemId = random(RegionNewItems),
            {ok, State#state{prev_state=PrevState#prev_state{item_id=RandomItemId}}}
    end;

%% VERIFIED
%% READ_SPECULA
run(view_item, _KeyGen, _ValueGen, State=#state{tx_server=TxServer, 
            part_list=PartList, prev_state=PrevState, node_id=MyNode, hash_dict=HashDict, specula=Specula}) ->
    ItemId = PrevState#prev_state.item_id,
    case ItemId of
        undef ->
            {prev_state, State};
        _ ->
            %lager:info("Item Id is ~w", [ItemId]),
            ItemKey = rubis_tool:get_key(ItemId, item), 
            {ItemNode, _} = ItemId,
            TxId = gen_server:call(TxServer, {start_tx, true, true}),
            Item = read_from_node(TxServer, TxId, ItemKey, ItemNode, MyNode, PartList, HashDict),
            {ItemNode, _} = Item#item.i_seller,
            SellerKey = rubis_tool:get_key(Item#item.i_seller, user),
            %lager:warning("Trying to read from seller key ~p", [SellerKey]),
            _User = read_from_node(TxServer, TxId, SellerKey, ItemNode, MyNode, PartList, HashDict),
            BidIds = Item#item.i_bid_ids,
            lists:foreach(fun(BidId) ->
                            BidKey = rubis_tool:get_key(BidId, bid),
                            %{BidNode, _} = BidId,
                            %case BidNode == (MyNode+4-2) rem 4 + 1 of
                            %    true -> lager:warning("Bid unreplicated! ItemKey is ~p, BidList is ~p", [ItemKey, BidIds]); 
                            %    false -> ok
                            %end,
                            %lager:warning("Trying to read bid ~p from node ~w", [BidKey, BidNode]),
                            _Bid = read_from_node(TxServer, TxId, BidKey, ItemNode, MyNode, PartList, HashDict)
                          end, BidIds),
            case Specula of
                true ->
                    _ =  gen_server:call(TxServer, {certify, TxId, [], []}, ?TIMEOUT);
                _ ->
                    ok
            end,
            {ok, State#state{prev_state=PrevState#prev_state{last_user_id=Item#item.i_seller}}}
    end;

%%% VERIFIED 
%% READ_SPECULA
run(view_user_info, _KeyGen, _ValueGen, State=#state{prev_state=PrevState, tx_server=TxServer,
        hash_dict=HashDict, node_id=MyNode, part_list=PartList, specula=Specula, 
        num_replicates=NumReplicates, num_nodes=NumNodes}) ->
    UserId = PrevState#prev_state.last_user_id,
    case UserId of
        undef ->
            {prev_state, State};
        _ ->
            {UserNode, _} = UserId,
            %lager:info("UerId is ~w", [UserId]),
            UserKey = rubis_tool:get_key(UserId, user), 
            TxId = gen_server:call(TxServer, {start_tx}),
            %lager:warning("Trying to read from user key ~p", [UserKey]),
            User = read_from_node(TxServer, TxId, UserKey, UserNode, MyNode, PartList, HashDict),
            NumComments = User#user.u_num_comments,
            case NumComments of 0 ->  
                            case Specula of
                                true ->
                                    _ =  gen_server:call(TxServer, {certify, TxId, [], []}, ?TIMEOUT);
                                _ ->
                                    ok
                            end,
                            {ok, State};
                _ ->
                    %ToFetchCommentList = lists:seq(max(NumComments - ?COMMENT_NUM+1, 1), NumComments),
                    CommentNodeList = User#user.u_comment_nodes,
                    RandCommentId = case CommentNodeList of [] -> 0;
                                        _ -> random:uniform(length(CommentNodeList)) %random(ToFetchCommentList), 
                                    end,
                    {_, _, RandUser} = lists:foldl(fun(Node, {CId, I, RU}) ->
                                CommentKey = rubis_tool:get_key({UserId, CId}, comment),
                                Comment = case replicate_node(Node, MyNode, NumReplicates, NumNodes) of true -> 
                                            read_from_node(TxServer, TxId, CommentKey, Node, MyNode, PartList, HashDict);
                                            false ->
                                            %lager:warning("WTF, do not replicate?? ~w, i am ~w, user node ~w", [Node, MyNode, UserNode]),
                                            read_from_node(TxServer, TxId, CommentKey, UserNode, MyNode, PartList, HashDict)
                                          end,
                                case Comment of [] -> 
                                            lager:warning("Empty comment trying to read from ~w", [UserId]),
                                            {CId-1, I+1, RU};
                                    _ ->
                                    case I of RandCommentId -> {CId-1, I+1, Comment#comment.c_from_id};
                                                            _ -> {CId-1, I+1, RU}
                                    end
                                end 
                                end, {User#user.u_num_comments, 1, UserId}, CommentNodeList),
                    case Specula of
                        true ->
                            _ =  gen_server:call(TxServer, {certify, TxId, [], []}, ?TIMEOUT);
                        _ ->
                            ok
                    end,
                    {ok, State#state{prev_state=PrevState#prev_state{last_user_id=RandUser}}}
            end
    end;

%% VERIFIED
%% READ_SPECULA
run(view_bid_history, _KeyGen, _ValueGen, State=#state{tx_server=TxServer, 
            part_list=PartList, prev_state=PrevState, node_id=MyNode, hash_dict=HashDict, specula=Specula}) ->
    ItemId = PrevState#prev_state.item_id,
    {ItemNode, _} = ItemId,
    ItemKey = rubis_tool:get_key(ItemId, item), 
    TxId = gen_server:call(TxServer, {start_tx}),
    Item = read_from_node(TxServer, TxId, ItemKey, ItemNode, MyNode, PartList, HashDict),
    ItemBids = Item#item.i_bid_ids,
    case ItemBids of
        [] ->
            case Specula of
                true ->
                    _ =  gen_server:call(TxServer, {certify, TxId, [], []}, ?TIMEOUT);
                _ ->
                    ok
            end,
            {ok, State};
        _ ->
            RandBidId = random(ItemBids),
            RandUserId = lists:foldl(fun(B, A) ->
                        BidKey = rubis_tool:get_key(B, bid),
                        %{BidNode, _} = B,
                        %case BidNode == (MyNode+4-2) rem 4 + 1 of
                        %    true -> lager:warning("Bid unreplicated! ItemKey is ~p, BidList is ~p", [ItemKey, ItemBids]); 
                        %    false -> ok
                        %end,
                        %lager:warning("Trying to read bid ~p from node ~w", [BidKey, BidNode]),
                        Bid = read_from_node(TxServer, TxId, BidKey, ItemNode, MyNode, PartList, HashDict),
                        %UserKey = rubis_tool:get_key(Bid#bid.b_user_id, user),
                        %lager:warning("Trying to read from user key ~w", [UserKey]),
                        %_ = read_from_node(TxServer, TxId, UserKey, MyNode, MyNode, PartList, HashDict),
                        case RandBidId of B -> Bid#bid.b_user_id;
                                 _ -> A
                        end
                        end, undef, ItemBids),
            case Specula of
                true ->
                    _ =  gen_server:call(TxServer, {certify, TxId, [], []}, ?TIMEOUT);
                _ ->
                    ok
            end,
            {ok, State#state{prev_state=PrevState#prev_state{last_user_id=RandUserId}}}
    end;

%% VERIFIED
run(buy_now_auth, _KeyGen, _ValueGen, State=#state{tx_server=TxServer}) ->
    %lager:info("Mhuahau, buy now auth"),
    TxId = gen_server:call(TxServer, {start_tx}),
    _ =  gen_server:call(TxServer, {certify, TxId, [], []}, ?TIMEOUT),
    {ok, State};

%% Buy now should be placed as the user's location
%% VERIFIED
%% READ_SPECULA
run(buy_now, _KeyGen, _ValueGen, State=#state{tx_server=TxServer,
              part_list=PartList, prev_state=PrevState, node_id=MyNode, hash_dict=HashDict, specula=Specula}) -> 
    ItemId = PrevState#prev_state.item_id,
    ItemKey = rubis_tool:get_key(ItemId, item),
    {ItemNode, _} = ItemId,
    TxId = gen_server:call(TxServer, {start_tx}),
    Item = read_from_node(TxServer, TxId, ItemKey, ItemNode, MyNode, PartList, HashDict),
    SellerKey = rubis_tool:get_key(Item#item.i_seller, user),
    %lager:warning("Trying to read from user key ~w", [SellerKey]),
    _Seller = read_from_node(TxServer, TxId, SellerKey, ItemNode, MyNode, PartList, HashDict),
    %BidIds = Item#item.i_bid_ids,
    %lists:foreach(fun(BidId) ->
    %                BidKey = rubis_tool:get_key(BidId, bid),
    %                _Bid = read_from_node(TxServer, TxId, BidKey, MyNode, MyNode, PartList, HashDict)
    %              end, BidIds),
    case Specula of
        true ->
            _ =  gen_server:call(TxServer, {certify, TxId, [], []}, ?TIMEOUT);
        _ ->
            ok
    end,
    {ok, State#state{prev_state=PrevState#prev_state{max_qty=Item#item.i_quantity}}};

%% VERIFIED
%% No need to change anything here
run(store_buy_now, _KeyGen, _ValueGen, State=#state{part_list=PartList, tx_server=TxServer, prev_state=PrevState, 
        hash_dict=HashDict, specula=Specula, node_id=MyNode}) ->
    MyselfId = PrevState#prev_state.myself_id,
    ItemId = PrevState#prev_state.item_id,
    {ItemNode, _} = ItemId,
    %% Qty always <= MaxQty!!!
    %MaxQty = Request#request.max_qty,

    TxId = gen_server:call(TxServer, {start_tx, true, true}),
    ItemKey = rubis_tool:get_key(ItemId, item),
    Item = read_from_node(TxServer, TxId, ItemKey, ItemNode, MyNode, PartList, HashDict),
    OldQuantity = Item#item.i_quantity,
    case OldQuantity of
        0 ->
            case Specula of
                true ->
                    _ =  gen_server:call(TxServer, {certify, TxId, [], []}, ?TIMEOUT);
                _ ->
                    ok
            end,
            {ok, State};
        _ ->
            Qty = random:uniform(Item#item.i_quantity),
            Now = rubis_tool:now_nsec(),
            NewItem = case OldQuantity of
                            Qty -> Item#item{i_quantity=0, i_end_date=Now};
                            _ -> Item#item{i_quantity=OldQuantity-Qty}
                          end,

            WS = dict:new(),
            WS1 = dict:store({ItemNode, ItemKey}, NewItem, WS), 
            BuyNowIdKey = rubis_tool:get_key(MyNode, lastbuynow),
            BuyNowId = read_from_node(TxServer, TxId, BuyNowIdKey, MyNode, MyNode, PartList, HashDict),
            BuyNowNextId = BuyNowId + 1, 
            BuyNow = rubis_tool:create_buy_now(MyselfId, ItemId, Qty, Now), 
            BuyNowKey = rubis_tool:get_key({MyNode, BuyNowNextId}, buy_now),

            %% Update what the user has bought
            MyselfKey = rubis_tool:get_key(MyselfId, user),
            %lager:warning("Trying to read from user key ~p", [MyselfKey]),
            Myself = read_from_node(TxServer, TxId, MyselfKey, MyNode, MyNode, PartList, HashDict),
            RemainList0 = lists:sublist(Myself#user.u_bought, ?BOUGHT_NUM), 
            Myself1 = Myself#user{u_bought= [ItemId|RemainList0]},
            
            WS2 = dict:store({MyNode, BuyNowKey}, BuyNow, WS1), 
            WS3 = dict:store({MyNode, BuyNowIdKey}, BuyNowNextId, WS2), 
            WS4 = dict:store({MyNode, MyselfKey}, Myself1, WS3), 

            {LocalWriteList, RemoteWriteList} = get_local_remote_writeset(WS4, PartList, MyNode),
            Response =  gen_server:call(TxServer, {certify, TxId, LocalWriteList, RemoteWriteList}, ?TIMEOUT),%, length(DepsList)}),
            RT3 = os:timestamp(),
            case Response of
                {ok, {committed, _}} ->
                    {ok, State};
                {ok, {specula_commit, _}} ->
                    {ok, State};
                {aborted, _} ->
                    random:seed(RT3),
                    {error, aborted, State};
                {badrpc, Reason} ->
                    {error, Reason, State}
            end
    end;

%% VERIFIED
run(put_bid_auth, _KeyGen, _ValueGen, State=#state{tx_server=TxServer}) ->
    TxId = gen_server:call(TxServer, {start_tx}),
    _ =  gen_server:call(TxServer, {certify, TxId, [], []}, ?TIMEOUT),
    %lager:info("Mhuahau, put bid auth"),
    {ok, State};

%% KINDA VERIFIED, MAYBE NEED TO FETCH MORE BIDS
%% READ_SPECULA
run(put_bid, _KeyGen, _ValueGen, State=#state{part_list=PartList, tx_server=TxServer, prev_state=PrevState, 
        hash_dict=HashDict, node_id=MyNode, specula=Specula}) ->
    %lager:warning("In put_bid, state is ~w", [PrevState]),
    ItemId = PrevState#prev_state.item_id,
    case ItemId of
        undef -> {prev_state, State};
        _ ->
            TxId = gen_server:call(TxServer, {start_tx}),
            {ItemNode, _} = ItemId,
            ItemKey = rubis_tool:get_key(ItemId, item),
            %MyselfKey = rubis_tool:get_key(MyselfId, user),
            %lager:warning("Trying to read from user key ~p", [UserKey]),
            Item = read_from_node(TxServer, TxId, ItemKey, ItemNode, MyNode, PartList, HashDict),
            SellerId = Item#item.i_seller,
            SellerKey = rubis_tool:get_key(SellerId, user),
            {ItemNode, _} = SellerId,
            %lager:warning("Trying to read key of seller ~w", [SellerKey]),
            _Seller = read_from_node(TxServer, TxId, SellerKey, ItemNode, MyNode, PartList, HashDict),
            case Specula of
                true ->
                    _ =  gen_server:call(TxServer, {certify, TxId, [], []}, ?TIMEOUT);
                _ ->
                    ok
            end,
            {ok, State#state{prev_state=PrevState#prev_state{min_bid=Item#item.i_max_bid+1}}}
    end;

%% VERIFIED
run(store_bid, _KeyGen, _ValueGen, State=#state{part_list=PartList, tx_server=TxServer, 
        hash_dict=HashDict, node_id=MyNode, prev_state=PrevState, specula=Specula}) ->
    MyselfId = PrevState#prev_state.myself_id,
    ItemId = PrevState#prev_state.item_id,
    MinBid = PrevState#prev_state.min_bid,
    AddBid = random:uniform(10),
    Bid = MinBid + AddBid,
    MaxBid = Bid + AddBid,

    {ItemNode, _} = ItemId,
    %%% Qty should be smaller than maxQty, maxBid >= minBid, bid >= minBid, maxBid >= Bid 

    WS = dict:new(),
    TxId = gen_server:call(TxServer, {start_tx, true, true}),

    ItemKey = rubis_tool:get_key(ItemId, item),
	Item = read_from_node(TxServer, TxId, ItemKey, ItemNode, MyNode, PartList, HashDict),
    ItemQty = Item#item.i_quantity,
    case ItemQty of 0 ->
            case Specula of
                true ->
                    _ =  gen_server:call(TxServer, {certify, TxId, [], []}, ?TIMEOUT);
                _ ->
                    ok
            end,
            {ok, State};
        _ ->
            Qty = random:uniform(ItemQty),

            BidIdKey = rubis_tool:get_key(MyNode, lastbid),
            BidId = read_from_node(TxServer, TxId, BidIdKey, MyNode, MyNode, PartList, HashDict),
            BidNextId = BidId + 1, 
            Now = rubis_tool:now_nsec(),
            BidObj = rubis_tool:create_bid(MyselfId, ItemId, Qty, Bid, MaxBid, Now),
            BidKey = rubis_tool:get_key({MyNode, BidNextId}, bid),

            WS0 = dict:store({MyNode, BidIdKey}, BidNextId, WS),
            WS1 = dict:store({MyNode, BidKey}, BidObj, WS0),
            WS2 = case MyNode of ItemNode -> WS1;
                                        _ -> dict:store({ItemNode, BidKey}, BidObj, WS0)
                  end,
            
            %% Add bid to bidding item
            ExistBids = Item#item.i_bid_ids,
            RemainList = lists:sublist(ExistBids, ?BID_NUM), 
            NewItem = case Item#item.i_max_bid < Bid of
                true -> 
                        Item#item{i_max_bid=Bid, i_nb_of_bids=Item#item.i_nb_of_bids+1, i_bid_ids=[{MyNode, BidNextId}|RemainList]};
                false -> Item#item{i_nb_of_bids=Item#item.i_nb_of_bids+1, i_bid_ids=[{MyNode, BidNextId}|RemainList]}
            end,  
            WS3 = dict:store({ItemNode, ItemKey}, NewItem, WS2),

            %% Add bid to the bidding user
            MyselfKey = rubis_tool:get_key(MyselfId, user),
            %lager:warning("Trying to read from user key ~p", [MyselfKey]),
            Myself = read_from_node(TxServer, TxId, MyselfKey, MyNode, MyNode, PartList, HashDict),
            RemainList0 = lists:sublist(Myself#user.u_bids, ?BID_NUM), 
            Myself1 = Myself#user{u_bids= [{MyNode, BidNextId}|RemainList0]},
            WS4 = dict:store({MyNode, MyselfKey}, Myself1, WS3),

            {LocalWriteList, RemoteWriteList} = get_local_remote_writeset(WS4, PartList, MyNode),
            %DepsList = ets:lookup(dep_table, TxId),
            Response =  gen_server:call(TxServer, {certify, TxId, LocalWriteList, RemoteWriteList}, ?TIMEOUT),%, length(DepsList)}),
            case Response of
                {ok, {committed, _}} ->
                    {ok, State};
                {ok, {specula_commit, _}} ->
                    {ok, State};
                {error,timeout} ->
                    {error, aborted, State};
                {aborted, _} ->
                    {error, aborted, State};
                {badrpc, Reason} ->
                    {error, Reason, State}
            end
    end;

run(put_comment_auth, _KeyGen, _ValueGen, State=#state{tx_server=TxServer}) ->
    TxId = gen_server:call(TxServer, {start_tx}),
    _ =  gen_server:call(TxServer, {certify, TxId, [], []}, ?TIMEOUT),
    %lager:info("Mhuahau, put comment auth"),
    {ok, State};

%% VERIFIED
%% READ_SPECULA
run(put_comment, _KeyGen, _ValueGen, State=#state{part_list=PartList, tx_server=TxServer, 
        hash_dict=HashDict, node_id=MyNode, prev_state=PrevState, specula=Specula}) ->
    ToUserId = PrevState#prev_state.last_user_id,
    ItemId = PrevState#prev_state.item_id,
    case (ToUserId == undef) or (ItemId == undef) of
        true ->
            {prev_state, State};
        _ ->
            ToUserKey = rubis_tool:get_key(ToUserId, user),
            {ItemNode, _} = ItemId,
            {ToUserNode, _} = ToUserId,
            ItemKey = rubis_tool:get_key(ItemId, item),
            %lager:warning("Trying to read from user key ~w", [FromUserKey]),
            TxId = gen_server:call(TxServer, {start_tx}),
            %lager:warning("Trying to read from user key ~p", [ToUserKey]),
            _ToUser = read_from_node(TxServer, TxId, ToUserKey, ToUserNode, MyNode, PartList, HashDict),
            _Item = read_from_node(TxServer, TxId, ItemKey, ItemNode, MyNode, PartList, HashDict),
            case Specula of
                true ->
                    _ =  gen_server:call(TxServer, {certify, TxId, [], []}, ?TIMEOUT);
                _ ->
                    ok
            end,
            {ok, State}
    end;

%% The comment should be placed in the partition of the user who receives the comment
%% VERIFIED
run(store_comment, _KeyGen, _ValueGen, State=#state{part_list=PartList, tx_server=TxServer, 
        hash_dict=HashDict, node_id=MyNode, prev_state=PrevState}) ->
    ItemId = PrevState#prev_state.item_id,
    ToId = PrevState#prev_state.last_user_id, 
    MyselfId = PrevState#prev_state.myself_id, 
    {MyselfNode, _} = MyselfId, 

    Comments = ["Very bad", "Bad", "Normal", "Good", "Very good"],
    Rating = random:uniform(5) - 3,
    Comment = lists:nth(Rating+3, Comments),

    TxId = gen_server:call(TxServer, {start_tx, true, true}),
    {ToNode, _} = ToId,
    ToIdKey = rubis_tool:get_key(ToId, user), 
    %lager:warning("Trying to read from user key ~w", [ToIdKey]),
	ToUser = read_from_node(TxServer, TxId, ToIdKey, ToNode, MyNode, PartList, HashDict),
    NumComments = ToUser#user.u_num_comments,
    NextCommentId = NumComments + 1,
    
    Now = rubis_tool:now_nsec(),
    CommentKey = rubis_tool:get_key({ToId, NextCommentId}, comment),
    CommentObj = rubis_tool:create_comment(MyselfId, ToId, ItemId, Rating, Now, Comment),

	WS1 = dict:store({ToNode, CommentKey}, CommentObj, dict:new()),
	WS2 = dict:store({MyselfNode, CommentKey}, CommentObj, WS1),
    
    CommentNodes = ToUser#user.u_comment_nodes, 
    RemainList = lists:sublist(CommentNodes, ?COMMENT_NUM), 

    NewToUser = ToUser#user{u_rating=ToUser#user.u_rating+Rating, u_num_comments=NumComments+1, u_comment_nodes=[MyselfNode|RemainList]},

	WS3 = dict:store({ToNode, ToIdKey}, NewToUser, WS2),

    {LocalWriteList, RemoteWriteList} = get_local_remote_writeset(WS3, PartList, MyNode),
    %DepsList = ets:lookup(dep_table, TxId),
    Response =  gen_server:call(TxServer, {certify, TxId, LocalWriteList, RemoteWriteList}, ?TIMEOUT),%, length(DepsList)}),
    case Response of
        {ok, {committed, _}} ->
            {ok, State};
        {ok, {specula_commit, _}} ->
            {ok, State};
        {error,timeout} ->
            lager:info("Timeout on client ~p",[TxServer]),
            {error, aborted, State};
        {aborted, _} ->
            {error, aborted, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end;

run(sell, _KeyGen, _ValueGen, State) ->
    {ok, State};

run(select_category_to_sell_item, _KeyGen, _ValueGen, State=#state{nb_categories=NBCategories, tx_server=TxServer, 
            hash_dict=HashDict, part_list=PartList, node_id=MyNode, specula=Specula}) ->
    Seq = lists:seq(1, NBCategories),
    TxId = gen_server:call(TxServer, {start_tx}),
    lists:foreach(fun(N) ->
                CategoryKey = rubis_tool:get_key(N, category),
                _ = read_from_node(TxServer, TxId, CategoryKey, MyNode, MyNode, PartList, HashDict)
                end, Seq),
    case Specula of
        true ->
            _ =  gen_server:call(TxServer, {certify, TxId, [], []}, ?TIMEOUT);
        _ ->
            ok
    end,
    {ok, State};

run(sell_item_form, _KeyGen, _ValueGen, State=#state{nb_categories=NBCategories}) ->
    _RandCategory = random:uniform(NBCategories),
    {ok, State};

%% When a user registers an item, the item should be placed in the same server as the user
%% VERIFIED
%% May register an item that is stored in other places
run(register_item, _KeyGen, _ValueGen, State=#state{tx_server=TxServer, node_id=MyNode, prev_state=PrevState, 
            part_list=PartList, hash_dict=HashDict, max_duration=MaxDuration, nb_categories=NBCategories, max_quantity=MaxQuantity,
            percent_reserve_item=PercentReserveItem, percent_buy_now=PercentBuyNow, percent_unique_item=PercentUniqueItem}) ->
            %access_master=AccessMaster, access_slave=AccessSlave, dc_rep_ids=DcRepIds, no_rep_ids=DcNoRepIds}) ->
    %ItemNode = pick_node(MyNode, DcRepIds, DcNoRepIds, AccessMaster, AccessSlave),
    ItemNode = MyNode,

    Description = "Don't buy it!",
    InitialPrice = random:uniform(5000),
    CategoryId = random:uniform(NBCategories),
    MyNodeRegion = PrevState#prev_state.region,
    ReservePrice = case random:uniform(100) < PercentReserveItem of
                        true -> random:uniform(1000) + InitialPrice;
                        false -> 0
                    end,

    BuyNow = case random:uniform(100) < PercentBuyNow of
                  true -> random:uniform(1000) + InitialPrice + ReservePrice;
                  false -> 0
              end,

    Duration = random:uniform(MaxDuration),
    Quantity = case random:uniform(100) < PercentUniqueItem of
                    true -> 1;
                    false -> MaxQuantity 
                end,

    StartDate = rubis_tool:now_nsec(),
    EndDate = StartDate + Duration,
    LocalItemIdKey = rubis_tool:get_key(ItemNode, lastitem), 
    {_, RegionId} = MyNodeRegion,
    CategoryNewItemsKey = rubis_tool:get_key({ItemNode, CategoryId}, categorynewitems), 
    RegionNewItemsKey = rubis_tool:get_key({ItemNode, RegionId}, regionnewitems), 

    TxId = gen_server:call(TxServer, {start_tx, true, true}),
    LocalItemId = read_from_node(TxServer, TxId, LocalItemIdKey, ItemNode, MyNode, PartList, HashDict),
    LocalNextItemId = LocalItemId + 1,

    ItemId = {ItemNode, LocalNextItemId},
    ItemKey = rubis_tool:get_key(ItemId, item),
    Name = "RUBiSAutoItem#" ++ integer_to_list(LocalItemId),
    Item = rubis_tool:create_item(Name, Description, InitialPrice, Quantity, ReservePrice, BuyNow, 
            StartDate, EndDate, PrevState#prev_state.myself_id, CategoryId),

    %% Add to the user's selling list
    MyselfKey = rubis_tool:get_key(PrevState#prev_state.myself_id, user),
    %lager:warning("Trying to read user ~p", [MyselfKey]),
    Myself = read_from_node(TxServer, TxId, MyselfKey, MyNode, MyNode, PartList, HashDict),
    RemainList0 = lists:sublist(Myself#user.u_sellings, ?SELLING_NUM), 
    %lager:info("Remaing list is ~w", [RemainList0]),
    Myself1 = Myself#user{u_sellings= [ItemId|RemainList0]},

    CategoryNewItems0 = read_from_node(TxServer, TxId, CategoryNewItemsKey, ItemNode, MyNode, PartList, HashDict),
    %lager:warning("Category new items are ~w", [CategoryNewItems0]),
    CategoryNewItems = case CategoryNewItems0 of
                        empty ->  [ItemId];
                        _ ->  CategoryRemainList0 = lists:sublist(CategoryNewItems0, ?CATEGORY_NEW_ITEMS),
                              [ItemId|CategoryRemainList0]
                       end,

    RegionNewItems0 = read_from_node(TxServer, TxId, RegionNewItemsKey, ItemNode, MyNode, PartList, HashDict),
    %lager:warning("Region new items are ~w", [RegionNewItems0]),
    RegionNewItems = case RegionNewItems0 of
                        empty ->  [ItemId];
                        _ ->  RegionRemainList0 = lists:sublist(RegionNewItems0, ?REGION_NEW_ITEMS),
                              [ItemId|RegionRemainList0]
                     end,

    %lager:info("Category ~p, new items are ~w", [CategoryNewItemsKey, CategoryNewItems]),
    %lager:info("Region ~p, new items are ~w", [RegionNewItemsKey, RegionNewItems]),

    WS1 = dict:store({ItemNode, LocalItemIdKey}, LocalNextItemId, dict:new()), 
    WS2 = dict:store({ItemNode, ItemKey}, Item, WS1),
    WS3 = dict:store({MyNode, MyselfKey}, Myself1, WS2),
    WS4 = dict:store({ItemNode, CategoryNewItemsKey}, CategoryNewItems, WS3),
    WS5 = dict:store({ItemNode, RegionNewItemsKey}, RegionNewItems, WS4),

    {LocalWriteList, RemoteWriteList} = get_local_remote_writeset(WS5, PartList, MyNode),
    Response =  gen_server:call(TxServer, {certify, TxId, LocalWriteList, RemoteWriteList}, ?TIMEOUT),%, length(DepsList)}),
    case Response of
        {ok, {committed, _}} ->
            {ok, State};
        {ok, {specula_commit, _}} ->
            {ok, State};
        {error,timeout} ->
            lager:info("Timeout on client ~p",[TxServer]),
            {error, aborted, State};
        {aborted, _} ->
            {error, aborted, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end;

run(about_me_auth, _KeyGen, _ValueGen, State=#state{tx_server=TxServer}) ->
    TxId = gen_server:call(TxServer, {start_tx}),
    _ =  gen_server:call(TxServer, {certify, TxId, [], []}, ?TIMEOUT),
    %lager:info("Mhuahau, abou me auth"),
    {ok, State};

%% READ_SPECULA
run(about_me, _KeyGen, _ValueGen, State=#state{tx_server=TxServer, node_id=MyNode, prev_state=PrevState,
              specula=Specula, part_list=PartList, hash_dict=HashDict}) ->
    TxId = gen_server:call(TxServer, {start_tx, true, true}),
    %% Display user info
    MyselfId = PrevState#prev_state.myself_id, 
    MyselfKey = rubis_tool:get_key(MyselfId, user),
    %lager:info("Trying to read myself ~p", [MyselfKey]),
    Myself = read_from_node(TxServer, TxId, MyselfKey, MyNode, MyNode, PartList, HashDict),
    NBids = length(Myself#user.u_bids),
    NSellItems = length(Myself#user.u_sellings),
    NBoughtItems = length(Myself#user.u_bought),
    NumComments = min(Myself#user.u_num_comments, ?COMMENT_NUM), %%length(User#user.u_comments),
    TotalUsers = NBids + NBoughtItems + NumComments, 
    TotalItems = NBids + NSellItems + NBoughtItems, 
    RandItemIndex = 
        case TotalItems of 0 -> 0;
                           _ -> random:uniform(TotalItems)
        end,
    RandUserIndex = 
        case TotalUsers of 0 -> 0;
                          _ -> random:uniform(TotalUsers)
        end,
    %% List bids
    %% List won items
    {CI1, CU1, RI1, RU1} = lists:foldl(fun(BI, {IncI, IncU, RI, RU}) ->
        BidKey = rubis_tool:get_key(BI, bid), 
        %{BidNode, _} = BI,
        %case BidNode == (MyNode+4-2) rem 4 + 1 of
        %    true -> lager:warning("Bid unreplicated! User bids are ~p", [Myself#user.u_bids]); 
        %    false -> ok
        %end,
        %lager:warning("Trying to read bid ~p from node ~w", [BidKey, BidNode]),
        Bid = read_from_node(TxServer, TxId, BidKey, MyNode, MyNode, PartList, HashDict),
        ItemI = Bid#bid.b_item_id,
        {ItemNode, _} = ItemI,
        ItemKey = rubis_tool:get_key(ItemI, item), 
        Item = read_from_node(TxServer, TxId, ItemKey, ItemNode, MyNode, PartList, HashDict),
        SellerI = Item#item.i_seller,
        {SellerNode, _} = SellerI,
        SellerKey = rubis_tool:get_key(SellerI, user), 
        %lager:warning("Trying to read user ~p", [SellerKey]),
        _Seller = read_from_node(TxServer, TxId, SellerKey, SellerNode, MyNode, PartList, HashDict),
        case IncI of RandItemIndex -> 
            case IncU of RandUserIndex -> {IncI+1, IncU+1, ItemI, SellerI}; 
                                 _ -> {IncI+1, IncU+1, ItemI, RU}
            end;
            _ -> 
                case IncU of RandUserIndex -> {IncI+1, IncU+1, RI, SellerI}; 
                                    _ -> {IncI+1, IncU+1, RI, RU}
                end
        end
        end, {1, 1, PrevState#prev_state.item_id, PrevState#prev_state.last_user_id}, Myself#user.u_bids),
    %% List items
    {CI2, RI2} = lists:foldl(fun(ItemIndex, {IncI, RI}) ->
        ItemKey = rubis_tool:get_key(ItemIndex, item), 
        {ItemNode, _} = ItemIndex,
        _Item = read_from_node(TxServer, TxId, ItemKey, ItemNode, MyNode, PartList, HashDict),
        case IncI of RandItemIndex ->  {IncI+1, ItemIndex}; 
                             _ ->  {IncI+1, RI}
        end
        end, {CI1, RI1}, Myself#user.u_sellings),
    %% List bought items
    {_CI3, CU2, RI3, RU2} = lists:foldl(fun(II, {IncI, IncU, RI, RU}) ->
        ItemKey = rubis_tool:get_key(II, item), 
        {ItemNode, _} = II,
        Item = read_from_node(TxServer, TxId, ItemKey, ItemNode, MyNode, PartList, HashDict),
        SellerI = Item#item.i_seller,
        SellerKey = rubis_tool:get_key(SellerI, user), 
        {ItemNode, _} = SellerI,
        _Seller = read_from_node(TxServer, TxId, SellerKey, ItemNode, MyNode, PartList, HashDict),
        case IncI of RandItemIndex -> 
            case IncU of RandUserIndex -> {IncI+1, IncU+1, II, SellerI}; 
                                 _ -> {IncI+1, IncU+1, II, RU}
            end;
            _ -> 
                case IncU of RandUserIndex -> {IncI+1, IncU+1, RI, SellerI}; 
                                    _ -> {IncI+1, IncU+1, RI, RU}
                end
        end
        end, {CI2, CU1, RI2, RU1}, Myself#user.u_bought),
    %% List comments
    ToFetchCommentList = lists:seq(max(1, NumComments-?COMMENT_NUM+1), NumComments), 
    %lager:info("ToFetchList is ~w", [ToFetchCommentList]),
    {_, RU3} = lists:foldl(fun(CI, {IncU, RU}) ->
        %lager:warning("CI is ~w", [CI]),
        CommentKey = rubis_tool:get_key({MyselfId, CI}, comment), 
        %lager:warning("Trying to read comment ~p, user node is ~w", [CommentKey, MyNode]),
        Comment = read_from_node(TxServer, TxId, CommentKey, MyNode, MyNode, PartList, HashDict),
        case Comment of [] -> {IncU+1, RU};
                        _ ->
                        case IncU of RandUserIndex ->  {IncU+1, Comment#comment.c_from_id}; 
                                             _ ->  {IncU+1, RU}
                        end
        end
        end, {CU2, RU2}, ToFetchCommentList),
    case Specula of
        true ->
            _ =  gen_server:call(TxServer, {certify, TxId, [], []}, ?TIMEOUT);
        _ ->
            ok
    end,
    {ok, State#state{prev_state=PrevState#prev_state{last_user_id=RU3, item_id=RI3}}}.

get_partition(Key, PartList, HashLength) ->
    Num = crypto:bytes_to_integer(erlang:md5(Key)) rem HashLength +1,
    lists:nth(Num, PartList).

pick_node(MyId, DcRepIds, DcNoRepIds, AccessMaster, AccessRep) ->
    %lager:info("~w ~w ~w ~w ~w ~w", [MyId, RepIds, SlaveRepIds, WPerNode, AccessMaster, AccessRep]),
    R = random:uniform(100),
    case R =< AccessMaster of
        true ->
            MyId;
        false ->
            case R =< AccessMaster + AccessRep of
                true ->
                    L = length(DcRepIds),
                    case L of 0 ->
                                N = R rem length(DcNoRepIds) + 1,
                                lists:nth(N, DcNoRepIds);
                            _ ->
                                N = R rem L + 1,
                                lists:nth(N, DcRepIds)
                    end;
                false ->
                    L = length(DcNoRepIds),
                    case L of 0 ->
                                N = R rem length(DcRepIds) + 1,
                                lists:nth(N, DcRepIds);
                            _ ->
                                N = R rem L + 1,
                                lists:nth(N, DcNoRepIds)
                    end
            end
    end.


    
index(Elem, L) ->
    index(Elem, L, 1).

index(_, [], _) ->
    -1;
index(E, [E|_], N) ->
    N;
index(E, [_|L], N) ->
    index(E, L, N+1).

empty_read_from_node(TxServer, TxId, Key, Node, MyNode, PartList, HashDict) ->
    {ok, V} = case Node of
        MyNode ->
            {_, L} = lists:nth(Node, PartList),
            Index = crypto:bytes_to_integer(erlang:md5(Key)) rem length(L) + 1,
            Part = lists:nth(Index, L),
            ets:update_counter(list_to_atom(pid_to_list(TxServer)), master, 1),
            gen_server:call(TxServer, {read, Key, TxId, Part}, ?READ_TIMEOUT);
        _ ->
            case dict:find(Node, HashDict) of
                error ->
                    %lager:error("WTF, Reading from cache!!! Key is ~p, Node is ~p, MyNode is ~w, HashDict is ~w", [Key, Node, MyNode, dict:to_list(HashDict)]),
                    {_, L} = lists:nth(Node, PartList),
                    Index = crypto:bytes_to_integer(erlang:md5(Key)) rem length(L) + 1,
                    Part = lists:nth(Index, L),
                    CacheServName = dict:fetch(cache, HashDict), 
                    ets:update_counter(list_to_atom(pid_to_list(TxServer)), remote, 1),
                    gen_server:call(CacheServName, {read, Key, TxId, Part}, ?READ_TIMEOUT);
                {ok, N} ->
                    {_, L} = lists:nth(Node, PartList),
                    Index = crypto:bytes_to_integer(erlang:md5(Key)) rem length(L) + 1,
                    Part = lists:nth(Index, L),
                    ets:update_counter(list_to_atom(pid_to_list(TxServer)), slave, 1),
                    gen_server:call(N, {read, Key, TxId, Part}, ?READ_TIMEOUT)
            end
    end,
    V.

read_from_node(TxServer, TxId, Key, Node, MyNode, PartList, HashDict) ->
    {ok, V} = case Node of
        MyNode ->
            {_, L} = lists:nth(Node, PartList),
            Index = crypto:bytes_to_integer(erlang:md5(Key)) rem length(L) + 1,
            Part = lists:nth(Index, L),
            ets:update_counter(list_to_atom(pid_to_list(TxServer)), master, 1),
            gen_server:call(TxServer, {read, Key, TxId, Part}, ?READ_TIMEOUT);
        _ ->
            case dict:find(Node, HashDict) of
                error ->
                    %lager:error("WTF, Reading from cache!!! Key is ~p, Node is ~p", [Key, Node]),
                    %lager:error("WTF, Reading from cache!!! Key is ~p, Node is ~p, MyNode is ~w, HashDict is ~w", [Key, Node, MyNode, dict:to_list(HashDict)]),
                    {_, L} = lists:nth(Node, PartList),
                    Index = crypto:bytes_to_integer(erlang:md5(Key)) rem length(L) + 1,
                    Part = lists:nth(Index, L),
                    CacheServName = dict:fetch(cache, HashDict), 
                    ets:update_counter(list_to_atom(pid_to_list(TxServer)), remote, 1),
                    gen_server:call(CacheServName, {read, Key, TxId, Part}, ?READ_TIMEOUT);
                {ok, N} ->
                    {_, L} = lists:nth(Node, PartList),
                    Index = crypto:bytes_to_integer(erlang:md5(Key)) rem length(L) + 1,
                    Part = lists:nth(Index, L),
                    ets:update_counter(list_to_atom(pid_to_list(TxServer)), slave, 1),
                    gen_server:call(N, {read, Key, TxId, Part}, ?READ_TIMEOUT)
            end
    end,
    case V of
        [] ->
            lager:error("Key ~p not found!!!! Should read from dc ~w, my dc is ~w,~w, txserver is ~w", [Key, Node, MyNode, node(), TxServer]),
            V;
        _ ->
            V
    end.
    %case Res of
    %    {specula, DepTx} ->
    %        ets:insert(dep_table, {TxId, DepTx});
    %    ok ->
    %        ok
    %end,

read(TxServer, TxId, Key, ExpandPartList, HashLength) ->
    Part = get_partition(Key, ExpandPartList, HashLength),
    {ok, V} = gen_server:call(TxServer, {read, Key, TxId, Part}, ?READ_TIMEOUT),
    case V of
        [] ->
            lager:error("Key ~p not found!!!!", [Key]),
            error;
        _ ->
            %lager:info("Reading ~p, ~p", [Key, V]),
            V
    end.

get_local_remote_writeset(WriteSet, PartList, MyNode) ->
    {LWSD, RWSD} = dict:fold(fun({N, Key}, Value, {LWS, RWS}) ->
                    case N of MyNode -> {add_to_writeset(Key, Value, lists:nth(MyNode, PartList), LWS), RWS};
                               _ -> {LWS, add_to_writeset(Key, Value, lists:nth(N, PartList), RWS)}
                    end end, {dict:new(), dict:new()}, WriteSet),
    {dict:to_list(LWSD), dict:to_list(RWSD)}.

add_to_writeset(Key, Value, {_, PartList}, WSet) ->
    Index = crypto:bytes_to_integer(erlang:md5(Key)) rem length(PartList) + 1,
    Part = lists:nth(Index, PartList),
    %lager:info("Adding  ~p, ~p to ~w", [Key, Value, Part]),
    dict:append(Part, {Key, Value}, WSet).

random(L) ->
    Len = length(L),
    lists:nth(random:uniform(Len), L).

load_config() ->
    ConfigFile = basho_bench_config:get(config_file), 
    TermsList =
        case file:consult(ConfigFile) of
              {ok, Terms} ->
                  Terms;
              {error, Reason} ->
                  lager:info("Failed to parse config file ~s: ~p\n", [ConfigFile, Reason])
          end,
    load_config(TermsList, dict:new()).

load_config([], D) ->
    D;
load_config([{Key, Value} | Rest], D) ->
    D1 = dict:store(Key, Value, D),
    load_config(Rest, D1);
load_config([ Other | Rest], D) ->
    io:format("Ignoring non-tuple config value: ~p\n", [Other]),
    load_config(Rest, D).

replicate_node(MyNode, MyNode, _, _) -> 
    true;
replicate_node(Node, MyNode, NumReplicates, NumAllNodes) -> 
    case MyNode < Node of true -> (Node-MyNode) =< NumReplicates;
                          false -> (Node+NumAllNodes-MyNode) =< NumReplicates
    end.
