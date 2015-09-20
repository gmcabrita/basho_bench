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
                pb_pid,
                num_updates,
                num_txns,
                op_type,
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
    PbPort = basho_bench_config:get(antidote_pb_port),
    Types  = basho_bench_config:get(antidote_types),
    NumPartitions = length(IPs),
    NumUpdates = basho_bench_config:get(num_updates), 
    KeyGenMode = basho_bench_config:get(key_gen_mode), 
    NumTxns = basho_bench_config:get(num_txns), 
    OpType = basho_bench_config:get(op_type), 

    %% Choose the node using our ID as a modulus
    TargetNode = lists:nth((Id rem length(IPs)+1), IPs),
    ?INFO("Using target node ~p for worker ~p\n", [TargetNode, Id]),

    {ok, Pid} = antidotec_pb_socket:start_link(TargetNode, PbPort),
    TypeDict = dict:from_list(Types),
    {ok, #state{time={1,1,1}, worker_id=Id,
               pb_pid = Pid,
	           num_partitions = NumPartitions,
               num_updates = NumUpdates,
               num_txns = NumTxns,
               op_type = OpType,
               type_dict = TypeDict, pb_port=PbPort,
               key_gen_mode=KeyGenMode,
               target_node=TargetNode}}.

%% @doc Read a key
run(read, KeyGen, _ValueGen, State=#state{pb_pid = Pid, worker_id = Id, pb_port=Port, target_node=Node, type_dict=TypeDict}) ->
    Key = KeyGen(),
    Type = get_key_type(Key, TypeDict),
    Response =  antidotec_pb_socket:get_crdt(Key, Type, Pid),
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
    State=#state{type_dict=TypeDict,
                 pb_pid = Pid,
                 worker_id = Id,
                 pb_port=Port,
                 target_node=Node}) ->
    Key = KeyGen(),
    %%TODO: Support for different data types
    Type = get_key_type(Key, TypeDict),
    {Mod, Op, Param} = get_random_param(TypeDict, Type, ValueGen()),
    Obj = Mod:Op(Param, Mod:new(Key)),
    Response = antidotec_pb_socket:store_crdt(Obj, Pid),
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
                target_node=Node, num_txns=NumTxns, num_updates=NumUpdates, pb_port=Port, pb_pid=Pid}) ->
    Operations = generate_list_of_txns(NumTxns, NumUpdates, TypeDict, Id, ValueGen, OpType, KeyGenMode), 
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
                target_node=Node, num_txns=NumTxns, num_updates=NumUpdates, pb_port=Port, pb_pid=Pid}) ->
    Operations=generate_txns_by_id(NumTxns, NumUpdates, TypeDict, Id, ValueGen), 
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

generate_list_of_txns(NumTxn, NumUpdates, TypeDict, Id, ValueGen, OpType, random) ->
    _Base = NumTxn*NumUpdates*(Id-1),
    M = lists:seq(0, NumTxn-1),
    GenerateOp = fun(Key1, AccList) ->
                    Type1 = get_key_type(Key1, TypeDict),
                    %ByteKey = list_to_binary(integer_to_list(Key1)),
                    {_Mod1, Op1, KeyParam1} = get_random_param(TypeDict, Type1, Id, ValueGen()),
                    case OpType of
                        update ->
                            [{update, Key1, Type1, Op1, KeyParam1}|AccList];
                        read ->
                            [{read, Key1, Type1}|AccList];
                        all ->
                            [{read, Key1, Type1}, {update, Key1, Type1, Op1, KeyParam1}|AccList]
                    end
                 end,
    lists:map(fun(_) ->
                        FList=lists:foldl(GenerateOp, [], d_rand(20000,NumUpdates,[])), FList end, M);
generate_list_of_txns(NumTxn, NumUpdates, TypeDict, Id, ValueGen, OpType, by_id) ->
    Base = NumTxn*NumUpdates*(Id-1),
    L = lists:seq(0, NumUpdates-1),
    M = lists:seq(0, NumTxn-1),
    GenerateOp = fun(_, {Acc, AccList}) ->
                    Key1 = Acc, %random:uniform(20000),
                    Type1 = get_key_type(Key1, TypeDict),
                    %ByteKey = list_to_binary(integer_to_list(Key1)),
                    {_Mod1, Op1, KeyParam1} = get_random_param(TypeDict, Type1, Id, ValueGen()),
                    case OpType of
                        update ->
                            {Acc+1,[{update, Key1, Type1, Op1, KeyParam1}|AccList]};
                        read ->
                            {Acc+1,[{read, Key1, Type1}|AccList]};
                        all ->
                            {Acc+1,[{read, Key1, Type1}, {update, Key1, Type1, Op1, KeyParam1}|AccList]}
                    end
                 end,
    lists:map(fun(Init) -> {_, FList}=lists:foldl(GenerateOp, {Base+Init*NumUpdates,[]}, L), FList end, M);
generate_list_of_txns(NumTxn, NumUpdates, TypeDict, Id, ValueGen, OpType, Interval) ->
    Base = Id,
    L = lists:seq(0, NumUpdates-1),
    M = lists:seq(0, NumTxn-1),
    GenerateOp = fun(_, {Acc, AccList}) ->
                    Key1 = Acc, %random:uniform(20000),
                    Type1 = get_key_type(Key1, TypeDict),
                    %ByteKey = list_to_binary(integer_to_list(Key1)),
                    {_Mod1, Op1, KeyParam1} = get_random_param(TypeDict, Type1, Id, ValueGen()),
                    case OpType of
                        update ->
                            {Acc+Interval,[{update, Key1, Type1, Op1, KeyParam1}|AccList]};
                        read ->
                            {Acc+Interval,[{read, Key1, Type1}|AccList]};
                        all ->
                            {Acc+Interval,[{read, Key1, Type1}, {update, Key1, Type1, Op1, KeyParam1}|AccList]}
                    end
                 end,
    lists:map(fun(_) -> {_, FList}=lists:foldl(GenerateOp, {Base,[]}, L), FList end, M).

generate_txns_by_id(NumTxn, NumUpdates, TypeDict, Id, ValueGen) ->
    case Id rem 2 of
        1 ->
            generate_list_of_txns(NumTxn, NumUpdates, TypeDict, Id, ValueGen, update, by_id); 
        0 ->
            generate_list_of_txns(NumTxn, NumUpdates, TypeDict, Id-1, ValueGen, read, by_id) 
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
    
    

