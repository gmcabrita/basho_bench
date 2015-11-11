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
-module(basho_bench_driver_antidote_pb).

-export([new/1,
         run/4]).

-include("basho_bench.hrl").

-define(TIMEOUT, 20000).
-record(state, {worker_id,
                time,
                type_dict,
                part_list,
                target_index,
                pb_pid,
                num_updates,
                num_reads,
                key_range,
                op_type,
                my_tx_server,
		        num_partitions,
                key_gen_mode,
                pb_port,
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
    NumPartitions = length(IPs),
    MyNode = basho_bench_config:get(antidote_mynode),
    NumUpdates = basho_bench_config:get(num_updates), 
    Cookie = basho_bench_config:get(antidote_cookie),
    NumReads = basho_bench_config:get(num_reads), 
    KeyRange = basho_bench_config:get(key_range), 
    KeyGenMode = basho_bench_config:get(key_gen_mode), 
    OpType = basho_bench_config:get(op_type), 

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

%    PbPort = lists:nth((Id rem length(PbPorts)+1), PbPorts),
    ?INFO("Using target node ~p for worker ~p\n", [TargetNode, Id]),
    Result = net_adm:ping(TargetNode),
    ?INFO("Result of ping is ~p \n", [Result]),
    MyTxServer = list_to_atom(atom_to_list(TargetNode) ++ "-cert-" ++ integer_to_list(Id)),
    %{ok, Pid} = antidotec_pb_socket:start_link(TargetNode, PbPort),
    %{ok, PartList} = antidotec_pb_socket:get_hash_fun(Pid),
    {PartList, _} =  rpc:call(TargetNode, hash_fun, get_hash_fun, []), %gen_server:call({global, MyTxServer}, {get_hash_fun}),
    {_, IntPart} = lists:foldl(fun({_, List}, {Num, Acc}) ->  
                            {Num+1, Acc++[{Num, List}]} end, {0, []}, PartList),
    lager:info("Part list is ~w",[IntPart]),
    TargetIndex = case TargetNode of
                    'dev1@127.0.0.1' -> 3;
                    'dev2@127.0.0.1' -> 2;
                    'dev3@127.0.0.1' -> 1;
                    8087 -> 1
                  end,
    {ok, #state{time={1,1,1}, worker_id=Id,
               my_tx_server=MyTxServer,
               part_list=IntPart,
	           num_partitions = NumPartitions,
               num_updates = NumUpdates,
               num_reads = NumReads,
               key_range = KeyRange,
               op_type = OpType,
               key_gen_mode=KeyGenMode,
               target_index=TargetIndex,
               target_node=TargetNode}}.

%% @doc Read a key
run(read, KeyGen, _ValueGen, State=#state{pb_pid = Pid, worker_id = Id, pb_port=Port, target_node=Node}) ->
    Key = KeyGen(),
    Response =  antidotec_pb_socket:general_tx([{read, Key}], Pid),
    case Response of
        {ok, _Value} ->
            {ok, State};
        {error,timeout} ->
            lager:info("Timeout on client ~p",[Id]),
            antidotec_pb_socket:stop(Pid),
            {ok, NewPid} = antidotec_pb_socket:start_link(Node, Port),
            {error, timeout, State#state{pb_pid=NewPid}    };            
        {error, Reason} ->
            lager:error("Error: ~p",[Reason]),
            {error, Reason, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end;

%% @doc Read a key
run(test, _KeyGen, _ValueGen, State=#state{pb_pid = Pid, worker_id = Id, pb_port=Port, target_node=Node,
                    num_updates=NumUpdates}) ->
    {MegaSecs, Secs, MicroSecs} = now(),
    Time = (MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs,
    Updates = [{random:uniform(20000), 1} || _  <- lists:seq(1,NumUpdates)],
    Response =  antidotec_pb_socket:prepare(Time, whatever, Updates, Pid),
    case Response of
        {ok, _Value} ->
            {ok, State};
        {error,timeout} ->
            lager:info("Timeout on client ~p",[Id]),
            antidotec_pb_socket:stop(Pid),
            {ok, NewPid} = antidotec_pb_socket:start_link(Node, Port),
            {error, timeout, State#state{pb_pid=NewPid}    };            
        {error, Reason} ->
            lager:error("Error: ~p",[Reason]),
            {error, Reason, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end;


run(single_read, _KeyGen, _ValueGen, State=#state{pb_pid = Pid, worker_id = Id, pb_port=Port, target_node=Node}) ->
    PartitionId = random:uniform(4),
    Key = random:uniform(20000),
    Response =  antidotec_pb_socket:single_read(Pid, {now_microsec(), self()}, Key, PartitionId),
    case Response of
        {ok, _Value} ->
            {ok, State};
        {error,timeout} ->
            lager:info("Timeout on client ~p",[Id]),
            antidotec_pb_socket:stop(Pid),
            {ok, NewPid} = antidotec_pb_socket:start_link(Node, Port),
            {error, timeout, State#state{pb_pid=NewPid}};            
        {error, Reason} ->
            lager:error("Error: ~p",[Reason]),
            {error, Reason, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end;

%% @doc Multikey txn 
run(start_tx, _KeyGen, _ValueGen, State=#state{pb_pid = Pid, worker_id = Id, pb_port=Port, target_node=Node}) ->
    Response = antidotec_pb_socket:start_tx(0, Pid),
    case Response of
        {ok, _Value} ->
            {ok, State};
        {error,timeout} ->
            lager:info("Timeout on client ~p",[Id]),
            antidotec_pb_socket:stop(Pid),
            {ok, NewPid} = antidotec_pb_socket:start_link(Node, Port),
            {error, timeout, State#state{pb_pid=NewPid}};            
        {error, Reason} ->
            lager:error("Error: ~p",[Reason]),
            {error, Reason, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end;

run(certify, _KeyGen, _ValueGen, State=#state{my_tx_server=MyTxServer, part_list=PartList, worker_id = Id, num_updates=NumUpdates, target_index=TargetIndex, key_range=Range, num_reads=NumReads}) ->
    TxId = gen_server:call({global, MyTxServer}, {start_tx}),
    %lager:info("TxId is ~w", [TxId]),
    Keys = generate_read_keys(PartList, NumReads, Range),
    lists:foreach(fun({Key, NodeId, PartId}) ->
                _ =  gen_server:call({global, MyTxServer}, {read, Key, TxId, {raw, NodeId, PartId}})
                    end, Keys),
    case NumUpdates of
        0 ->
            {ok, State};
        _ ->
            AvgUpNum = NumUpdates div length(PartList) +1,
            LocalUps = generate_ups(lists:nth(TargetIndex, PartList), AvgUpNum, Range),
            RemoteNodes = lists:sublist(PartList, TargetIndex-1) ++ lists:sublist(PartList, TargetIndex+1, length(PartList)),
            %lager:info("TargetIndex is ~w, RemoteNodes are ~w", [TargetIndex, RemoteNodes]),
            RemoteUps = case length(PartList) of
                                1 ->
                                    {1, []};
                                _ ->
                                    lists:foldl(fun(NodePart, Acc) ->  
                                        [generate_ups(NodePart, AvgUpNum, Range)|Acc] end, [], 
                                    RemoteNodes)
                            end,
            FRemoteUps = lists:flatten(RemoteUps),
            %lager:info("Remote up ~w", [FRemoteUps]),
            Response =  gen_server:call({global, MyTxServer}, {certify, TxId, {raw, LocalUps}, {raw, FRemoteUps}}),
            %Response =  antidotec_pb_socket:certify(Pid, {now_microsec(), self()}, LocalUps, FRemoteUps, Id),
            case Response of
                {ok, _Value} ->
                    {ok, State};
                {error,timeout} ->
                    lager:info("Timeout on client ~p",[Id]),
                    {error, timeout, State};            
                {aborted, _} ->
                    lager:error("Aborted"),
                    {error, aborted, State};
                {badrpc, Reason} ->
                    {error, Reason, State}
            end
    end;

%% @doc Multikey txn 
run(read_all_write_one, KeyGen, ValueGen, State=#state{pb_pid = Pid, worker_id = Id, num_partitions=NumPart, pb_port=Port, target_node=Node, type_dict=TypeDict}) ->
    KeyInt = KeyGen(),
    KeyList = lists:seq(KeyInt, KeyInt+NumPart-1), 
    KeyTypeList = get_list_key_type(KeyList, TypeDict, []),
    Response =  antidotec_pb_socket:snapshot_get_crdts(KeyTypeList, Pid),
    case Response of
        {ok, _, _} ->
    	    run(append, KeyGen, ValueGen, State);
        {error,timeout} ->
            lager:info("Timeout on client ~p",[Id]),
            antidotec_pb_socket:stop(Pid),
            {ok, NewPid} = antidotec_pb_socket:start_link(Node, Port),
            {error, timeout, State#state{pb_pid=NewPid}    };            
        {error, Reason} ->
            lager:error("Error: ~p",[Reason]),
            {error, Reason, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end;

run(single_up, KeyGen, ValueGen, State=#state{pb_pid = Pid, worker_id = Id, pb_port=Port, target_node=Node}) ->
    _KeyInt = KeyGen(),
    _Value = ValueGen(),
    PartitionId = random:uniform(8),
    Response =  antidotec_pb_socket:single_up_req(random_string(20),
             random_string(20), PartitionId, Pid),
    case Response of
        {ok, _} ->
            {ok, State};
        {error,timeout} ->
            lager:info("Timeout on client ~p",[Id]),
            antidotec_pb_socket:stop(Pid),
            {ok, NewPid} = antidotec_pb_socket:start_link(Node, Port),
            {error, timeout, State#state{pb_pid=NewPid}    };            
        {error, Reason} ->
            lager:error("Error: ~p",[Reason]),
            {error, Reason, State};
        error ->
            {error, abort, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end;

run(read_only, _KeyGen, _ValueGen, State=#state{pb_pid = Pid}) ->
    L = lists:seq(1,4),
    PartitionId = random:uniform(8),
    {ok, {ok, TxId}} =  antidotec_pb_socket:start_tx(0, Pid),
    ReadFun = fun(_) -> {ok, _} = antidotec_pb_socket:read(TxId, "Father"++integer_to_list(random:uniform(20000)), 
                            PartitionId, Pid) end,
    lists:foreach(ReadFun, L),
    {ok, State};

run(append_multiple, KeyGen, ValueGen, State=#state{pb_pid = Pid, worker_id = Id, pb_port=Port, target_node=Node, type_dict=TypeDict, num_updates=NumUpdates}) ->
    KeyInt = KeyGen(),
    KeyList = lists:seq(KeyInt, KeyInt+NumUpdates-1), 
    Value = ValueGen(),
    OpList = lists:foldl(fun(X, Acc) -> 
                                Type = get_key_type(X, TypeDict),
                                {Mod, Op, Param} = get_random_param(TypeDict, Type, Value),
                                Acc++[Mod:Op(Param, Mod:new(X))] 
                         end, [], KeyList),
    Response =  antidotec_pb_socket:atomic_store_crdts(OpList, Pid),
    case Response of
        {ok, _} ->
            {ok, State};
        {error,timeout} ->
            lager:info("Timeout on client ~p",[Id]),
            antidotec_pb_socket:stop(Pid),
            {ok, NewPid} = antidotec_pb_socket:start_link(Node, Port),
            {error, timeout, State#state{pb_pid=NewPid}    };            
        {error, Reason} ->
            lager:error("Error: ~p",[Reason]),
            {error, Reason, State};
        error ->
            {error, abort, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end;

%% @doc Write to a key
run(append, KeyGen, ValueGen,
    State=#state{
                 pb_pid = Pid,
                 worker_id = Id,
                 pb_port=Port,
                 target_node=Node}) ->
    Key = KeyGen(),
    %%TODO: Support for different data types
    Response = antidotec_pb_socket:general_tx([{update, Key, assign, ValueGen()}], Pid), 
    case Response of
        ok ->
            {ok, State};
        {ok, _Result} ->
            {ok, State};
        {error,timeout}->
            lager:info("Timeout on client ~p",[Id]),
            antidotec_pb_socket:stop(Pid),
            {ok, NewPid} = antidotec_pb_socket:start_link(Node, Port),
            {error, timeout, State#state{pb_pid=NewPid}}; 
        {error, Reason} ->
            %lager:error("Error: ~p",[Reason]),
            {error, Reason, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end;

%% @doc Write to a key
run(general_tx, _KeyGen, ValueGen, State=#state{worker_id=Id, type_dict=TypeDict, op_type=OpType, key_gen_mode=KeyGenMode, 
                target_node=Node, num_updates=NumUpdates, pb_port=Port, pb_pid=Pid}) ->
    Operations = generate_list_of_txns(NumUpdates, TypeDict, Id, ValueGen, OpType, KeyGenMode), 
    Response =  antidotec_pb_socket:general_tx(Operations, Pid),
    case Response of
        {ok, _} ->
            {ok, State};
        {error,timeout} ->
            lager:info("Timeout on client ~p, operations are ~p",[Id, Operations]),
            antidotec_pb_socket:stop(Pid),
            {ok, NewPid} = antidotec_pb_socket:start_link(Node, Port),
            {error, timeout, State#state{pb_pid=NewPid}};
        {error, Reason} ->
            {error, Reason, State};
        error ->
            {error, abort, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end;

run(workload_by_id, _KeyGen, ValueGen, State=#state{worker_id=Id, type_dict=TypeDict, 
                target_node=Node, num_updates=NumUpdates, pb_port=Port, pb_pid=Pid}) ->
    Operations=generate_txns_by_id(NumUpdates, TypeDict, Id, ValueGen), 
    Response =  antidotec_pb_socket:general_tx(Operations, Pid),
    case Response of
        {ok, _} ->
            {ok, State};
        {error,timeout} ->
            lager:info("Timeout on client ~p, operations are ~p",[Id, Operations]),
            antidotec_pb_socket:stop(Pid),
            {ok, NewPid} = antidotec_pb_socket:start_link(Node, Port),
            {error, timeout, State#state{pb_pid=NewPid}};
        {error, Reason} ->
            {error, Reason, State};
        error ->
            {error, abort, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end;

run(update, KeyGen, ValueGen,
    State=#state{type_dict=TypeDict,
                 pb_pid = Pid,
                 worker_id = Id,
                 pb_port=Port,
                 target_node=Node}) ->
    Key = KeyGen(),
    %%TODO: Support for different data types
    Type = get_key_type(Key, TypeDict),
    Response =  case antidotec_pb_socket:get_crdt(Key, Type, Pid) of
                    {ok, CRDT} ->
                        {Mod, Op, Param} = get_random_param(TypeDict, Type, ValueGen(), CRDT),
                        Obj = Mod:Op(Param,CRDT),
                        antidotec_pb_socket:store_crdt(Obj, Pid);
                    Other -> Other
                end,
    case Response of
        ok ->
            {ok, State};
        {ok, _Result} ->
            {ok, State};
        {error,timeout}->
            lager:info("Timeout on client ~p",[Id]),
            antidotec_pb_socket:stop(Pid),
            {ok, NewPid} = antidotec_pb_socket:start_link(Node, Port),
            {error, timeout, State#state{pb_pid=NewPid}}; 
        {error, Reason} ->
            {error, Reason, State};
        {badrpc, Reason} ->
            {error, Reason, State}
    end.


get_list_key_type([], _Dict, Acc) ->
    Acc;
get_list_key_type([Key|Rest], Dict, Acc) ->
    Pair = {Key, get_key_type(Key, Dict)},
    get_list_key_type(Rest, Dict, [Pair | Acc]).

get_key_type(Key, Dict) ->
    Keys = dict:fetch_keys(Dict),
    RanNum = Key rem length(Keys),
    lists:nth(RanNum+1, Keys).

get_random_param(Dict, Type, Value) ->
    Params = dict:fetch(Type, Dict),
    random:seed(now()),
    Num = random:uniform(length(Params)),
    case Type of
        riak_dt_pncounter ->
           {antidotec_counter,lists:nth(Num, Params), 1};
        riak_dt_orset ->
           {antidotec_set, lists:nth(Num, Params), Value}                     
    end.

get_random_param(Dict, Type, Value, Obj) ->
    Params = dict:fetch(Type, Dict),
    random:seed(now()),
    Num = random:uniform(length(Params)),
    case Type of
        riak_dt_pncounter ->
           {antidotec_counter,lists:nth(Num, Params), 1};
        riak_dt_orset ->
            Set = antidotec_set:value(Obj),
            Op = lists:nth(Num, Params),
            case Op of 
                remove ->
                    case sets:to_list(Set) of 
                        [] -> {antidotec_set, add, Value};                     
                        [H|_T] -> {antidotec_set, remove, H}
                    end;
                _ ->
                    {antidotec_set, Op, Value}
            end                      
    end.

generate_list_of_txns(NumUpdates, _TypeDict, _Id, _ValueGen, OpType, random) ->
    GenerateOp = fun(Key1, AccList) ->
                    %ByteKey = list_to_binary(integer_to_list(Key1)),
                    case OpType of
                        update ->
                            [{update, Key1, assign, 1}|AccList];
                        read ->
                            [{read, Key1}|AccList];
                        all ->
                            [{read, Key1}, {update, Key1, assign, 1}|AccList]
                    end
                 end,
    lists:foldl(GenerateOp, [], d_rand(20000,NumUpdates,[]));
generate_list_of_txns(NumUpdates, _TypeDict, Id, _ValueGen, OpType, by_id) ->
    Base = NumUpdates*(Id-1),
    L = lists:seq(0, NumUpdates-1),
    GenerateOp = fun(_, {Acc, AccList}) ->
                    Key1 = Acc, %random:uniform(20000),
                    case OpType of
                        update ->
                            {Acc+1,[{update, Key1, assign, 1}|AccList]};
                        read ->
                            {Acc+1,[{read, Key1}|AccList]};
                        all ->
                            {Acc+1,[{read, Key1}, {update, Key1, assign, 1}|AccList]}
                    end
                 end,
    {_, FList}=lists:foldl(GenerateOp, {Base,[]}, L),
    FList;
generate_list_of_txns(NumUpdates, _TypeDict, Id, _ValueGen, OpType, Interval) ->
    Base = Id,
    L = lists:seq(0, NumUpdates-1),
    GenerateOp = fun(_, {Acc, AccList}) ->
                    Key1 = Acc, %random:uniform(20000),
                    case OpType of
                        update ->
                            {Acc+Interval,[{update, Key1, assign, 1}|AccList]};
                        read ->
                            {Acc+Interval,[{read, Key1}|AccList]};
                        all ->
                            {Acc+Interval,[{read, Key1}, {update, Key1, assign, 1}|AccList]}
                    end
                 end,
    {_, FList}=lists:foldl(GenerateOp, {Base,[]}, L),
    FList.

generate_txns_by_id(NumUpdates, TypeDict, Id, ValueGen) ->
    case Id rem 2 of
        1 ->
            generate_list_of_txns(NumUpdates, TypeDict, Id, ValueGen, update, by_id); 
        0 ->
            generate_list_of_txns(NumUpdates, TypeDict, Id-1, ValueGen, read, by_id) 
    end.

d_rand(_, 0, Acc) ->
    Acc;
d_rand(Range, Num, Acc) ->
    Key = random:uniform(Range),
    case if_exist(Key, Acc) of
        true ->
            d_rand(Range, Num, Acc);
        false ->
            d_rand(Range, Num-1, [Key|Acc])
    end.
    
if_exist(_, []) ->
    false;
if_exist(N, [N|_L]) ->
    true;
if_exist(N, [_|L]) ->
    if_exist(N, L).

generate_ups({NodeIndex, PartNum}, TotalUpNum, Range) ->
    UpList = [random:uniform(PartNum) || _ <- lists:seq(1, TotalUpNum)],
    NewD = lists:foldl(fun(PartN, D) ->
                    case dict:find(PartN, D) of
                        {ok, V} ->
                            dict:store(PartN, V+1, D);
                        error ->
                            dict:store(PartN, 1, D)
                    end end,
                dict:new(), UpList),
    lists:map(fun({PartIndex, N}) ->  {NodeIndex, PartIndex, random_ups(N, NodeIndex, Range)} end, dict:to_list(NewD)).

generate_read_keys(PartList, TotalNum, Range) ->
    Len = length(PartList),
    L = lists:seq(1, TotalNum), 
    lists:foldl(fun(_, Acc) ->
            NodeNum = random:uniform(Len),
            {_, TotalPartNum} = lists:nth(NodeNum, PartList),
            PartNum = random:uniform(TotalPartNum),
            Key = integer_to_list(NodeNum) ++"-"++integer_to_list(random:uniform(Range)),
            [ {Key, NodeNum-1, PartNum}
                |Acc] end, [], L).

random_ups(K, Prefix, Range) ->
    L = lists:seq(1, K),
    {FL, _} = lists:foldl(fun(_, {Acc, Set}) ->
         R = uninum(Range, Set),
         { 
            [{integer_to_list(Prefix)++"-"++integer_to_list(R), fixed_string()}|Acc],
            sets:add_element(R, Set) }
         end, {[], sets:new()}, L),
    FL.

uninum(Range, Set) ->
    R = random:uniform(Range),
    case sets:is_element(R, Set) of
        true ->
            uninum(Range, Set);
        false ->
            R
    end.
        
fixed_string() ->
    "0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789".


random_string(Len) ->
    Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
    ChrsSize = size(Chrs),
    F = fun(_, R) -> [element(random:uniform(ChrsSize), Chrs) | R] end,
    lists:foldl(F, "", lists:seq(1, Len)).

now_microsec() ->
    {MegaSecs, Secs, MicroSecs} = now(),
    (MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs.


