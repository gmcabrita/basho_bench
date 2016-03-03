-module(locality_fun).

-include("tpcc.hrl").

-export([get_locality_list/5, get_pid/2, replace_name_by_pid/2]).

get_locality_list(PartList, ReplList, NumDcs, MyNode, single_dc_read) ->
    %% The first is the list of partitions that replicated by nodes in your dc and being the primary replicas. 
    %% The second list is the partitions that are replicated by nodes in your dc but not being the primary replicas.
    %[M] = [L || {N, L} <- ReplList, N == MyNode],
    AllNodes = [N || {N, _} <- PartList],
    NodeId = index(MyNode, AllNodes),
    OtherPrimaryIds = get_dc_other_node_ids(NodeId, AllNodes, NumDcs),

    %[M] = [L || {N, L} <- ReplList, N == MyNode],
    %MyRepIds = get_indexes(M, AllNodes),
    %lager:info("NoId is ~w, DcPRepId is ~w", [NodeId, DcPrimaryRepIds]),
    DcRepIds = get_replicas(OtherPrimaryIds++[NodeId], ReplList, AllNodes), 
    AllNodeIds = lists:seq(1, length(AllNodes)),
    DcNonRepIds = AllNodeIds -- ([NodeId] ++ OtherPrimaryIds ++ DcRepIds),
    %lager:info("NodeiD ~w, OthePId ~w, RepId ~w", [NodeId, OtherPrimaryIds, DcRepIds]),
    
    HashDict1 = build_dc_srep_dict(ReplList, AllNodes, NodeId, NumDcs),
    %HashDict = build_local_norep_dict(NodeId, ReplList, AllNodes, NoRepIds, NumDcs),
    %HashDict1 =  lists:foldl(fun(N, D) ->
    %                    dict:store(N, get_rep_name(MyNode, lists:nth(N, AllNodes)), D)
    %                    end, HashDict, MyRepIds),
    {OtherPrimaryIds, DcRepIds, DcNonRepIds, HashDict1};


get_locality_list(PartList, ReplList, NumDcs, MyNode, node_aware) ->
      [M] = [L || {N, L} <- ReplList, N == MyNode ],
      AllNodes = [N || {N, _} <- PartList],
      MyRepIds = get_indexes(M, AllNodes),
      NodeId = index(MyNode, AllNodes),
  
      NoRepList = (AllNodes -- M) -- [MyNode],
      NoRepIds = get_indexes(NoRepList, AllNodes),
      HashDict = build_local_norep_dict(NodeId, ReplList, AllNodes, NumDcs),
      HashDict1 =  lists:foldl(fun(N, D) ->
                          dict:store(N, get_rep_name(MyNode, lists:nth(N, AllNodes)), D)
                          end, HashDict, MyRepIds),
      {MyRepIds, NoRepIds, HashDict1}.

build_local_norep_dict(NodeId, ReplList, AllNodes, NumDcs) ->
    case length(AllNodes) of
        NumDcs -> dict:new();
        _ ->
            DcOtherNodesRep = get_dc_other_reps(NodeId, ReplList, AllNodes, NumDcs),
            %lager:info("DcOhternodes are ~w", [DcOtherNodes]),
            lists:foldl(fun({LocalNode, LocalRepNodes}, Dict) ->
                lists:foldl(fun(RepNode, D) ->
                RepId = index(RepNode, AllNodes),
                    %lager:info("Local node ~w replicates ~w", [LocalNode, RepNode]),
                dict:store(RepId, get_rep_name(LocalNode, RepNode), D)
                    end, Dict, LocalRepNodes)
                end, dict:new(), DcOtherNodesRep)
                %lager:info("nEWdICT IS ~w", [Dict]),
    end.

build_dc_srep_dict(ReplList, AllNodes, NodeId, NumDcs) ->
    DcOtherReps = get_dc_other_reps(NodeId, ReplList, AllNodes, NumDcs),
    %lager:info("Dcother reps are ~w" ,[DcOtherReps]),
    DcAllReps = DcOtherReps ++ [lists:nth(NodeId, ReplList)], 
    %lager:info("Dcother all reps are ~w" ,[DcAllReps]),
      lists:foldl(fun({LocalNode, LocalRepNodes}, Dict) ->
          lists:foldl(fun(RepNode, D) ->
          RepId = index(RepNode, AllNodes),
              %lager:info("Local node ~w replicates ~w", [LocalNode, RepNode]),
          dict:store(RepId, get_rep_name(LocalNode, RepNode), D)
              end, Dict, LocalRepNodes)
          end, dict:new(), DcAllReps).

delete_by_id(List, N) ->
  {L1, [_|L2]} = lists:split(N-1, List),
  L1 ++ L2.

index(Elem, L) ->
    index(Elem, L, 1).

index(_, [], _) ->
    -1;
index(E, [E|_], N) ->
    N;
index(E, [_|L], N) ->
    index(E, L, N+1).

get_rep_name(Target, Rep) ->
    list_to_atom(atom_to_list(Target)++"repl"++atom_to_list(Rep)).

get_indexes(PL, List) ->
    %lager:info("Trying to get index: PL ~w, List ~w", [PL, List]),
    [index(X, List) || X <- PL ].

get_dc_other_node_ids(NodeId, AllNodes, NumDcs) ->
    case length(AllNodes) of NumDcs -> [];
                      _->   NodesPerDc = length(AllNodes) div NumDcs,
                            DcId = (NodeId-1) div NodesPerDc+1,
                            Base = (DcId-1)*NodesPerDc,
                            DcNodes = lists:seq(Base+1, Base+NodesPerDc),
			                lists:delete(DcId, DcNodes)
    end.

get_dc_other_reps(NodeId, ReplList, AllNodes, NumDcs) ->
    case length(AllNodes) of NumDcs -> [];
                      _->   NodesPerDc = length(AllNodes) div NumDcs,
                            DcId = (NodeId-1) div NodesPerDc+1,
                            Base = (DcId-1)*NodesPerDc,
                            DcNodes = lists:sublist(ReplList, Base+1, NodesPerDc),
                            delete_by_id(DcNodes, NodeId-Base)
    end.

get_replicas(NodesId, ReplList, AllNodes) ->
    RS = lists:foldl(fun(NodeId, Set) ->
                   Node = lists:nth(NodeId, AllNodes), 
                   [M] = [L || {N, L} <- ReplList, N == Node],
                   lists:foldl(fun(N, S) -> sets:add_element(index(N, AllNodes), S) end, Set, M)
            end, sets:new(), NodesId),
    lager:info("Rs is ~w", [sets:to_list(RS)]),
    sets:to_list(RS).

get_pid(TargetNode, Name) ->
    rpc:call(TargetNode, tx_cert_sup, get_pid, [Name]).

replace_name_by_pid(TargetNode, Dict) ->
    dict:fold(fun(Key, Value, NewDict) ->
            Pid = case Key of cache -> rpc:call(TargetNode, tx_cert_sup, get_pid, [Value]);
			      _ -> rpc:call(TargetNode, tx_cert_sup, get_global_pid, [Value])
		  end,
            dict:store(Key, Pid, NewDict)
            end, dict:new(), Dict).
                    
