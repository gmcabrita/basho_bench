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

-define(TIMEOUT, 20000).
-define(READ_TIMEOUT, 20000).

-record(state, {worker_id,
                time,
                part_list,
		        my_table,
                process_time,
                expand_part_list,
                hash_length,
                prob_access,
                w_per_dc,
                c_c_last,
                c_c_id,
                c_ol_i_id,
                my_rep_list,
                to_sleep,
                hash_dict,
                deter,
                no_rep_list,
                other_master_ids,
                dc_rep_ids,
                no_rep_ids,
                num_nodes,
                master_num,
                slave_num,
                cache_num,
                master_range,
                slave_range,
                cache_range,
                local_hot_range,
                local_hot_rate,
                remote_hot_range,
                remote_hot_rate,
		        specula,
                local_commit,
                remote_commit,
                local_abort,
                remote_abort,
		        read,
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
    MasterToSleep = basho_bench_config:get(master_to_sleep),
    ToSleep = basho_bench_config:get(to_sleep),
    ProcessTime = basho_bench_config:get(process_time),
   
    MasterNum = basho_bench_config:get(master_num),
    SlaveNum = basho_bench_config:get(slave_num),
    CacheNum = basho_bench_config:get(cache_num),

    MasterRange = basho_bench_config:get(master_range),
    SlaveRange = basho_bench_config:get(slave_range),
    CacheRange = basho_bench_config:get(cache_range),

    ProbAccess = basho_bench_config:get(prob_access),

    LocalHotRange = basho_bench_config:get(local_hot_range),
    LocalHotRate = basho_bench_config:get(local_hot_rate),
    RemoteHotRange = basho_bench_config:get(remote_hot_range),
    RemoteHotRate = basho_bench_config:get(remote_hot_rate),
    Deter = basho_bench_config:get(deter),

    case net_kernel:start(MyNode) of
        {ok, _} ->
	        ok;
            %?INFO("Net kernel started as ~p\n", [node()]);
        {error, {already_started, _}} ->
            %?INFO("Net kernel already started as ~p\n", [node()]),
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

    case Id of 1 ->
                    timer:sleep(MasterToSleep);
              _ ->  timer:sleep(ToSleep)
    end,

    {PartList, ReplList, NumDcs} =  rpc:call(TargetNode, hash_fun, get_hash_fun, []), 
    %lager:info("Part list is ~w, repl list is ~w", [PartList, ReplList]),

    %[M] = [L || {N, L} <- ReplList, N == TargetNode ],
    AllNodes = [N || {N, _} <- PartList],
    NodeId = index(TargetNode, AllNodes),
    NumNodes = length(AllNodes),
    MyTxServer = locality_fun:get_pid(TargetNode, list_to_atom(atom_to_list(TargetNode) ++ "-cert-" ++ integer_to_list((Id-1) div length(IPs)+1))),
    ExpandPartList = lists:flatten([L || {_, L} <- PartList]),
    HashLength = length(ExpandPartList),
    {OtherMasterIds, DcRepIds, DcNoRepIds, HashDict} = locality_fun:get_locality_list(PartList, ReplList, NumDcs, TargetNode, single_dc_read),
    HashDict1 = locality_fun:replace_name_by_pid(TargetNode, dict:store(cache, TargetNode, HashDict)),
    lager:info("OtherMasterIds ~w, DcRepIds ~w, NoRepIds ~w, D ~w", [OtherMasterIds, DcRepIds, DcNoRepIds, dict:to_list(HashDict1)]),

    %lager:info("Part list is ~w",[PartList]),
    MyTable = ets:new(my_table, [private, set]),
    {ok, #state{time={1,1,1}, worker_id=Id,
               local_commit= {0,0},
               remote_commit = {0,0},
               local_abort= {0,0},
               remote_abort = {0,0},
               specula= {0,0},
               deter = Deter,
               read = 0,
               prob_access=ProbAccess,
               tx_server=MyTxServer,
               part_list = PartList,
		       my_table=MyTable,
               process_time=ProcessTime,
               other_master_ids=OtherMasterIds,
               dc_rep_ids = DcRepIds,
               no_rep_ids = DcNoRepIds,
               hash_dict = HashDict1,
               to_sleep=ToSleep,
               master_num=MasterNum,
               slave_num=SlaveNum,
               cache_num=CacheNum,
               master_range=MasterRange,
               slave_range=SlaveRange,
               cache_range=CacheRange,
               local_hot_range=LocalHotRange,
               local_hot_rate=LocalHotRate,
               remote_hot_range=RemoteHotRange,
               remote_hot_rate=RemoteHotRate,
               expand_part_list = ExpandPartList,
               hash_length = HashLength,   
               num_nodes = NumNodes,
               node_id = NodeId,
               target_node=TargetNode}}.

%% @doc Warehouse, District are always local.. Only choose to access local or remote objects when reading
%% objects. 
run(txn, _KeyGen, _ValueGen, State=#state{part_list=PartList, tx_server=TxServer, deter=Deter,
        dc_rep_ids=DcRepIds, node_id=MyNodeId,  hash_dict=HashDict, no_rep_ids=NoRepIds, prob_access=ProbAccess,
        local_hot_rate=LocalHotRate, local_hot_range=LocalHotRange, remote_hot_rate=RemoteHotRate, remote_hot_range=RemoteHotRange,
        master_num=MNum, slave_num=SNum, master_range=MRange, slave_range=SRange, local_commit=LocalCommit, local_abort=LocalAbort,
        remote_commit=RemoteCommit, remote_abort=RemoteAbort, specula=Specula, read=Read})->
    random:seed(os:timestamp()),
    Add = random:uniform(3)-2,

    TxId = gen_server:call(TxServer, {start_tx}),


    RT1 = os:timestamp(),
    WriteSet = case ProbAccess of 
                    f ->
                        MasterKeys = unique_keys(1, LocalHotRange, MRange,  MNum, LocalHotRate, MyNodeId),
                        SlaveRepKeys = unique_keys(MRange+1, RemoteHotRange, SRange, SNum, RemoteHotRate, DcRepIds),
                        WS1 = lists:foldl(fun({NodeId, Key}, WS) ->
                                    V = read_from_node(TxServer, TxId, Key, NodeId, MyNodeId, PartList, HashDict),
                                    dict:store({NodeId, Key}, V+Add, WS) 
                                end, dict:new(), MasterKeys),
                        lists:foldl(fun({NodeId, Key}, WS) ->
                                %lager:info("Remote Rep Dc id is ~w", [NodeId]),
                                V = read_from_node(TxServer, TxId, Key, NodeId, MyNodeId, PartList, HashDict),
                                dict:store({NodeId, Key}, V+Add, WS)
                                    end, WS1, SlaveRepKeys);
                    t ->
                        NumKeys = 15,
                        DcRepLen = length(DcRepIds),
                        NoRepLen = length(NoRepIds),
                        {_, WriteS} = lists:foldl(fun(_, {Ind, WS}) ->
                                    random:seed(os:timestamp()),  Rand = random:uniform(100),
                                    case Rand =< MNum of
                                        true -> Key =  random:uniform(MRange), %%hot_or_not(1, LocalHotRange, MRange, LocalHotRate),
                                                V = read_from_node(TxServer, TxId, Key, MyNodeId, MyNodeId, PartList, HashDict),
                                                {Ind, dict:store({MyNodeId, Key}, V+Add, WS)};
                                                false -> case Rand =< MNum+SNum of
                                                            true ->     
                                                                        Key = random:uniform(SRange)+MRange, %%hot_or_not(MRange+1, RemoteHotRange, SRange, RemoteHotRate),
                                                                        case Deter of
                                                                            false ->
                                                                                DcNode = lists:nth(Rand rem DcRepLen +1, DcRepIds),
                                                                                V = read_from_node(TxServer, TxId, Key, DcNode, MyNodeId, PartList, HashDict),
                                                                                {Ind+1, dict:store({DcNode, Key}, V+Add, WS)};
                                                                            _ ->
                                                                            DcNode = lists:nth(Ind rem Deter +1, DcRepIds),
                                                                            V = read_from_node(TxServer, TxId, Key, DcNode, MyNodeId, PartList, HashDict),
                                                                            {Ind+1, dict:store({DcNode, Key}, V+Add, WS)}
                                                                        end;
                                                            false -> OtherDcNode = lists:nth(Rand rem NoRepLen +1, NoRepIds),
                                                                     Key = hot_or_not(MRange+1, RemoteHotRange, SRange, RemoteHotRate),
                                                                     V = read_from_node(TxServer, TxId, Key, OtherDcNode, MyNodeId, PartList, HashDict),
                                                                     {Ind, dict:store({OtherDcNode, Key}, V+Add, WS)} 
                                                         end
                                             end
                                  end, {1, dict:new()}, lists:seq(1, NumKeys)),
                        WriteS
               end,

    RT2 = os:timestamp(),
                    
    {LocalWriteList, RemoteWriteList} = get_local_remote_writeset(WriteSet, PartList, MyNodeId),
    %%%%% WARNING: Local processing is turned off!!
    %local_process(ProcessTime),

    %lager:info("LW is ~p, RW is ~p",  [LocalWriteList, RemoteWriteList]),
    Response =  gen_server:call(TxServer, {certify, TxId, LocalWriteList, RemoteWriteList}, ?TIMEOUT),%, length(DepsList)}),
    RT3 = os:timestamp(),
    case Response of
        {ok, {committed, _}} ->
            %{ok, State};
	        case RemoteWriteList of
                [] ->
                    {LTime, LCount} = LocalCommit,
                    {ok, State#state{read=Read+get_time_diff(RT1, RT2), local_commit={LTime+get_time_diff(RT2, RT3),
                            LCount+1}}};
                _ ->
                    {RTime, RCount} = RemoteCommit,
                    {ok, State#state{read=Read+get_time_diff(RT1, RT2), remote_commit={RTime+get_time_diff(RT2, RT3),
                            RCount+1}}}
            end;
        {ok, {specula_commit, _}} ->
	        {STime, SCount} = Specula,
            {ok, State#state{read=Read+get_time_diff(RT1, RT2), specula={STime+get_time_diff(RT2, RT3),
                    SCount+1}}};
            %{ok, State};
        {error,timeout} ->
            lager:info("Timeout on client ~p",[TxServer]),
            {error, timeout, State};
        {aborted, local} ->
	        {LTime, LCount} = LocalAbort,
            {error, aborted, State#state{read=Read+get_time_diff(RT1, RT2), local_abort={LTime+get_time_diff(RT2, RT3),
                    LCount+1}}};
            %{error, aborted, State};
        {aborted, remote} ->
	        {RTime, RCount} = RemoteAbort,
            {error, aborted, State#state{read=Read+get_time_diff(RT1, RT2), remote_abort={RTime+get_time_diff(RT2, RT3),
                    RCount+1}}};
            %{error, aborted, State};
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

read_from_node(TxServer, TxId, Key, DcId, MyDcId, PartList, HashDict) ->
    {ok, V} = case DcId of
        MyDcId ->
            {_, L} = lists:nth(DcId, PartList),
            %Index = crypto:bytes_to_integer(erlang:md5(Key)) rem length(L) + 1,
            Index = Key rem length(L) + 1,
            Part = lists:nth(Index, L),
            gen_server:call(TxServer, {read, Key, TxId, Part}, ?READ_TIMEOUT);
        _ ->
            case dict:find(DcId, HashDict) of
                error ->
                    {_, L} = lists:nth(DcId, PartList),
                    %Index = crypto:bytes_to_integer(erlang:md5(Key)) rem length(L) + 1,
                    Index = Key rem length(L) + 1,
                    Part = lists:nth(Index, L),
                    CacheServName = dict:fetch(cache, HashDict),
                    gen_server:call(CacheServName, {read, Key, TxId, Part}, ?READ_TIMEOUT);
                {ok, N} ->
                    {_, L} = lists:nth(DcId, PartList),
                    %Index = crypto:bytes_to_integer(erlang:md5(Key)) rem length(L) + 1,
                    Index = Key rem length(L) + 1,
                    Part = lists:nth(Index, L),
                    gen_server:call(N, {read, Key, TxId, Part}, ?READ_TIMEOUT)
            end
    end,
    case V of
        [] ->
	    0;
        _ ->
            V
    end.
    %case Res of
    %    {specula, DepTx} ->
    %        ets:insert(dep_table, {TxId, DepTx});
    %    ok ->
    %        ok
    %end,

terminate(_, _State=#state{local_commit=Local, remote_commit=Remote, local_abort=LAbort, remote_abort=RAbort, 
           specula=Specula, read=Read}) ->
    {T1, C1} = Local,
    {T2, C2} = Remote,
    {T3, C3} = LAbort,
    {T4, C4} = RAbort,
    {T5, C5} = Specula,
    LCT = T1 div max(1, C1),
    RCT = T2 div max(1, C2),
    LA = T3 div max(1, C3),
    RA = T4 div max(1, C4),
    SCT = T5 div max(1, C5),
    NOR = Read div max(1, C1+C2+C3+C4+C5),
    File= "prep",
    %lager:info("File is ~p, Value is ~p, ~p, ~p, ~p, ~p, ~p, ~p", [File, NOPrep, NORead1, NORead2, NORead3, NOItem, PPrep, PRead]),
    file:write_file(File, io_lib:fwrite("~p ~p ~p  ~p  ~p ~p ~p ~p ~p ~p\n", 
            [NOR, LA,  RA, LCT, RCT, SCT, 0, 0,0,0]), [append]).

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

unique_keys(Start, HotRange, UniformRange, NumKeys, HotRate, Nodes) ->
    unique_keys(Start, HotRange, UniformRange, NumKeys, HotRate, sets:new(), Nodes). 

unique_keys(_, _, _, 0, _, Set, _) ->
    sets:to_list(Set);
unique_keys(Start, HotRange, UniformRange, NumKeys, HotRate, Set, Nodes) ->
    Key = hot_or_not(Start, HotRange, UniformRange, HotRate), 
    Node = case Nodes of [_|_] -> L = length(Nodes), Id = random:uniform(L), lists:nth(Id, Nodes);
                     _ ->  Nodes end,
    case sets:is_element({Node, Key}, Set) of
        false -> unique_keys(Start, HotRange, UniformRange, NumKeys-1, HotRate, sets:add_element({Node, Key}, Set), Nodes);
        true -> 
                unique_keys(Start, HotRange, UniformRange, NumKeys, HotRate, Set, Nodes)
    end.

hot_or_not(Start, HotRange, UniformRange, HotRate) ->
      random:seed(os:timestamp()),
      Rand = random:uniform(100),
      case Rand =< HotRate of
          true -> random:uniform(HotRange) + Start-1;
          false -> random:uniform(UniformRange-HotRange) + Start+HotRange-1
      end.

%unique_num(Base, Num, Range) ->
%    unique_num(Base, Num, Range, sets:new()).

%unique_num(_Base, 0, _Range, Set) ->
%    sets:to_list(Set);
%unique_num(Base, Num, Range, Set) ->
%    N = random:uniform(Range) + Base,
%    case sets:is_element(N, Set) of
%        false -> unique_num(Base, Num-1, Range, sets:add_element(N, Set));
%        true -> unique_num(Base, Num, Range, Set)
%    end.


%get_time_diff({A1, B1, C1}, {A2, B2, C2}) ->
 %   ((A2-A1)*1000000+ (B2-B1))*1000000+ C2-C1.


%local_process(0) ->
%    ok;
%local_process(N) ->
%    StartTime = os:timestamp(),
%    wait_until(StartTime, N*1000).

%wait_until(StartTime, Duration) ->
%    case timer:now_diff(os:timestamp(), StartTime) > Duration of
%        true ->
%            ok;
%        false ->
%            wait_until(StartTime, Duration)
%    end.

get_time_diff({A1, B1, C1}, {A2, B2, C2}) ->
    ((A2-A1)*1000000+ (B2-B1))*1000000+ C2-C1.
