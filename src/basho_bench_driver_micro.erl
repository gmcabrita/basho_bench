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
-module(basho_bench_driver_micro).

-export([new/1,
        terminate/2,
        run/4]).

-include("basho_bench.hrl").

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
                to_sleep,
                no_rep_list,
                my_rep_ids,
                no_rep_ids,
                num_dcs,
                master_num,
                slave_num,
                cache_num,
                master_range,
                slave_range,
                cache_range,
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
    MasterNum = basho_bench_config:get(master_num),
    SlaveNum = basho_bench_config:get(slave_num),
    CacheNum = basho_bench_config:get(cache_num),

    MasterRange = basho_bench_config:get(master_range),
    SlaveRange = basho_bench_config:get(slave_range),
    CacheRange = basho_bench_config:get(cache_range),

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

    {PartList, ReplList} =  rpc:call(TargetNode, hash_fun, get_hash_fun, []), 
    %lager:info("Part list is ~w, repl list is ~w", [PartList, ReplList]),

    [M] = [L || {N, L} <- ReplList, N == TargetNode ],
    AllDcs = [N || {N, _} <- PartList],
    MyRepIds = get_indexes(M, AllDcs),
    MyRepList = [{N, get_rep_name(TargetNode, lists:nth(N, AllDcs))} || N <- MyRepIds],
    lager:info("My Rep Ids is ~p, my rep list is ~p", [MyRepIds, MyRepList]),
    lager:info("My Rep Ids is ~p, my rep list is ~p", [MyRepIds, MyRepList]),
    DcId = index(TargetNode, AllDcs),
    NumDcs = length(AllDcs),
    MyTxServer = list_to_atom(atom_to_list(TargetNode) ++ "-cert-" ++ integer_to_list((Id-1) div length(IPs)+1)),
    %lager:info("MyTxServer is ~w", [MyTxServer]),

    %lager:info("All Dcs is ~p, dc id is ~w", [AllDcs, DcId]),
    NoRepList = (AllDcs -- M) -- [TargetNode],
    NoRepIds = get_indexes(NoRepList, AllDcs),
    lager:info("NoRep list is ~w, no rep ids is ~w", [NoRepList, NoRepIds]),

    ExpandPartList = lists:flatten([L || {_, L} <- PartList]),
    %lager:info("Ex list is ~w", [ExpandPartList]),
    HashLength = length(ExpandPartList),

    %lager:info("Part list is ~w",[PartList]),
    case Id of 1 -> 
    	    ets:new(meta_info, [public, named_table, set]),
		    ets:insert(meta_info, {payment, 0,0,0}),
		    ets:insert(meta_info, {new_order, 0,0,0});
	      _ -> ok 
    end,
    timer:sleep(ToSleep),
    {ok, #state{time={1,1,1}, worker_id=Id,
               tx_server=MyTxServer,
               access_master=AccessMaster,
               access_slave=AccessSlave,
               part_list = PartList,
               my_rep_list = MyRepList,
               my_rep_ids = MyRepIds,
               no_rep_list = NoRepList,
               to_sleep=ToSleep,
               master_num=MasterNum,
               slave_num=SlaveNum,
               cache_num=CacheNum,
               master_range=MasterRange,
               slave_range=SlaveRange,
               cache_range=CacheRange,
               no_specula={0,0},
               no_local={0,0},
               no_remote={0,0},
               no_local_abort={0,0},
               no_remote_abort={0,0},
               no_read=0,
               payment_prep={0,0}, 
               payment_read={0,0}, 
               no_rep_ids = NoRepIds,
               expand_part_list = ExpandPartList,
               hash_length = HashLength,   
               num_dcs = NumDcs,
               dc_id = DcId,
               target_node=TargetNode}}.

%% @doc Warehouse, District are always local.. Only choose to access local or remote objects when reading
%% objects. 
run(txn, _KeyGen, _ValueGen, State=#state{part_list=PartList, tx_server=TxServer, 
        my_rep_ids=MyRepIds, my_rep_list=MyRepList,  no_rep_ids=NoRepIds, dc_id=MyDcId, 
        master_num=MNum, slave_num=SNum, cache_num=CNum, master_range=MRange, slave_range=SRange, cache_range=CRange})->
    %LocalWS = dict:new(),
    %RemoteWS = dict:new(),
    MyRepSize = length(MyRepIds),
    NoRepSize = length(NoRepIds),
    random:seed(os:timestamp()),
    Add = random:uniform(100),

    TxId = gen_server:call({global, TxServer}, {start_tx}),

    MasterKeys = unique_num(1, MNum, MRange),
    SlaveKeys = unique_num(MRange+1, SNum, SRange),
    CacheKeys = unique_num(MRange+SRange+1, CNum, CRange),
    random:seed(os:timestamp()),
    WS1 = lists:foldl(fun(Key, WS) ->
                    V = read_from_node(TxServer, TxId, Key, MyDcId, MyDcId, PartList, MyRepList),
                    dict:store({MyDcId, Key}, V+Add, WS) 
                end, dict:new(), MasterKeys),
    WS2 = lists:foldl(fun(Key, WS) ->
                    NodeId = lists:nth(random:uniform(MyRepSize), MyRepIds),
                    V = read_from_node(TxServer, TxId, Key, NodeId, MyDcId, PartList, MyRepList),
                    dict:store({NodeId, Key}, V+Add, WS) 
                end, WS1, SlaveKeys),
    WS3 = lists:foldl(fun(Key, WS) ->
                    NodeId = lists:nth(random:uniform(NoRepSize), NoRepIds),
                    V = read_from_node(TxServer, TxId, Key, NodeId, MyDcId, PartList, MyRepList),
                    dict:store({NodeId, Key}, V+Add, WS) 
                end, WS2, CacheKeys),
                    
    {LocalWriteList, RemoteWriteList} = get_local_remote_writeset(WS3, PartList, MyDcId),
    %lager:info("LW is ~p, RW is ~p",  [LocalWriteList, RemoteWriteList]),
    Response =  gen_server:call({global, TxServer}, {certify, TxId, LocalWriteList, RemoteWriteList}, ?TIMEOUT),%, length(DepsList)}),
    case Response of
        {ok, {committed, _}} ->
            {ok, State};
        {ok, {specula_commit, _}} ->
            {ok, State};
        {error,timeout} ->
            lager:info("Timeout on client ~p",[TxServer]),
            {error, timeout, State};
        {aborted, local} ->
            {error, aborted, State};
        {aborted, remote} ->
            %{NORTime, NORCount} = NORAbort,
            {error, aborted, State};
        {aborted, _} ->
            {error, aborted, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end.
    
index(Elem, L) ->
    index(Elem, L, 1).

index(_, [], _) ->
    -1;
index(E, [E|_], N) ->
    N;
index(E, [_|L], N) ->
    index(E, L, N+1).

read_from_node(TxServer, TxId, Key, DcId, MyDcId, PartList, MyRepList) ->
    {ok, V} = case DcId of
        MyDcId ->
            {_, L} = lists:nth(DcId, PartList),
            Index = Key rem length(L) + 1,
            Part = lists:nth(Index, L),
            tx_cert_sup:read(TxServer, TxId, Key, Part);
        _ ->
            case get_replica(DcId, MyRepList) of
                false ->
                    {_, L} = lists:nth(DcId, PartList),
                    Index = Key rem length(L) + 1,
                    Part = lists:nth(Index, L),
                    {CacheServName, _} = lists:nth(MyDcId, PartList),
                    cache_serv:read(CacheServName, Key, TxId, Part);
                N ->
                    {_, L} = lists:nth(DcId, PartList),
                    Index = Key rem length(L) + 1,
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

terminate(_, _State) ->
    ok.

%terminate(_, _State=#state{no_local=NOLocal, no_remote=NORemote, no_local_abort=NOLAbort, no_remote_abort=NORAbort, 
%           no_specula=NOSpecula, no_read=NORead, payment_prep=PaymentPrep, payment_read=PaymentRead}) ->
%    {T1, C1} = NOLocal,
%    {T2, C2} = NORemote,
%    {T3, C3} = NOLAbort,
%    {T4, C4} = NORAbort,
%    {T5, C5} = NOSpecula,
%    LCT = T1 div max(1, C1),
%    RCT = T2 div max(1, C2),
%    LA = T3 div max(1, C3),
%    RA = T4 div max(1, C4),
%    SCT = T5 div max(1, C5),
%    NOR = NORead div max(1, C1+C2+C3+C4+C5),
%    {_PPrep, _PRead} = case PaymentPrep of
%			{0, 0} -> {0, 0};
%			{AccT1, AccN1} -> {AccRT1, AccN1} = PaymentRead,
%                                        {AccT1 div AccN1, AccRT1 div AccN1} 
%		     end,
%    File= "prep",
%    [{payment, PT, PP, PN}] = ets:lookup(meta_info, payment),
%    [{new_order, NT, NP, NN}] = ets:lookup(meta_info, new_order),
%    %lager:info("File is ~p, Value is ~p, ~p, ~p, ~p, ~p, ~p, ~p", [File, NOPrep, NORead1, NORead2, NORead3, NOItem, PPrep, PRead]),
%    file:write_file(File, io_lib:fwrite("~p ~p ~p  ~p  ~p ~p ~p ~p ~p ~p\n", 
%            [NOR, LA,  RA, LCT, RCT, SCT, PP/PT ,PN/PT, NP/NT, NN/NT]), [append]).

get_local_remote_writeset(WriteSet, PartList, LocalDcId) ->
    {LWSD, RWSD} = dict:fold(fun({NodeId, Key}, Value, {LWS, RWS}) ->
                    case NodeId of LocalDcId -> {add_to_writeset(Key, Value, lists:nth(LocalDcId, PartList), LWS), RWS};
                               _ -> {LWS, add_to_writeset(Key, Value, lists:nth(NodeId, PartList), RWS)}
                    end end, {dict:new(), dict:new()}, WriteSet),
    %L = dict:fetch_keys(RWSD),
    %NodeSet = lists:foldl(fun({_, N}, S) -> sets:add_element(N, S) end, sets:new(), L),
    %ets:update_counter(meta_info, TxType, [{2, 1}, {3, length(L)}, {4,sets:size(NodeSet)}]),
    {dict:to_list(LWSD), dict:to_list(RWSD)}.


add_to_writeset(Key, Value, {_, PartList}, WSet) ->
    Index = Key rem length(PartList) + 1,
    Part = lists:nth(Index, PartList),
    dict:append(Part, {Key, Value}, WSet).

get_indexes(PL, List) ->
    %lager:info("Trying to get index: PL ~w, List ~w", [PL, List]),
    [index(X, List) || X <- PL ].
    
get_replica(E, [{E, N}|_]) ->
    N;
get_replica(E, [_|L]) ->
    get_replica(E, L).

get_rep_name(Target, Rep) ->
    list_to_atom(atom_to_list(Target)++"repl"++atom_to_list(Rep)).

unique_num(Base, Num, Range) ->
    unique_num(Base, Num, Range, sets:new()).

unique_num(_Base, 0, _Range, Set) ->
    sets:to_list(Set);
unique_num(Base, Num, Range, Set) ->
    N = random:uniform(Range) + Base,
    case sets:is_element(N, Set) of
        false -> unique_num(Base, Num-1, Range, sets:add_element(N, Set));
        true -> unique_num(Base, Num, Range, Set)
    end.


