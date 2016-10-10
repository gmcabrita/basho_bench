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
-module(basho_bench_tuner).

-behaviour(gen_fsm).

-define(BIG, 8).
-define(SML, 0).
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, gather_stat/2, 
         binary_search/5,
         tuner_name/0,
         handle_info/3,
         handle_event/3,
         handle_sync_event/4,
         terminate/3, code_change/4]).


-record(state, { prev_throughput, 
                 my_workers=[],
                 myself,
                 master,
                 centralized,
                 %big,
                 %sml,
                 %mid,
                 previous,
                 current,
                 num_nodes,
                 all_nodes,
	    	 current_round,
		 round_dict,
                 remain_num,
                 sum_throughput = 0
               }).

-include("basho_bench.hrl").

%% ====================================================================
%% API
%% ====================================================================

start_link() ->
    Name = tuner_name(), 
    gen_fsm:start_link({global, Name}, ?MODULE, [Name], []).

%% ====================================================================
%% gen_server callbacks
%% ====================================================================

init([Name]) ->
    lager:warning("In init!!"),
    Myself = Name, 
    Centralized = basho_bench_config:get(centralized),
    %Sml = ?SML,
    %Big = ?BIG,
    %Mid = Sml,
    PrevThroughput = dict:new(), %lists:foldl(fun(N, D) ->
               % dict:store(N, inf, D)
               % end, dict:new(), lists:seq(Sml, Big)),
    case Centralized of
        false ->
            MyWorkers = basho_bench_sup:workers(),
            {ok, gather_stat, #state{ prev_throughput=PrevThroughput,
                            myself = Myself,
                            centralized = Centralized,
                            %sml = Sml,
                            %big = Big,
                            %mid = Mid,
                            previous = -1,
                            current = 0,
                            num_nodes=1,
                            current_round=1,
                            remain_num=1,
                            my_workers = MyWorkers,
                            all_nodes = [],
                            sum_throughput = 0}};
        true ->
            AllNodes = basho_bench_config:get(all_nodes),
            AllTuners = [ list_to_atom( get_ip(atom_to_list(Node), []) ++"auto_tuner") || Node <- AllNodes],
            NumNodes = length(AllNodes),
            [MasterNode|_] = AllNodes, 
            Master = list_to_atom( atom_to_list(MasterNode) ++ "auto_tuner"),
            MyWorkers = basho_bench_sup:workers(),
	        RoundDict = dict:store(1, {0, 0}, dict:new()),
            {ok, gather_stat, #state{ prev_throughput=PrevThroughput,
                            myself = Myself,
                            master = Master,
                            my_workers=MyWorkers,
                            centralized = Centralized,
                            num_nodes=NumNodes,
                            previous = -1,
                            current = 0,
                            remain_num=NumNodes,
                            current_round=1,
                            round_dict=RoundDict,
                            all_nodes = AllTuners,
                            sum_throughput = 0}}
    end.

gather_stat({throughput, Round, Throughput}, State=#state{remain_num=RemainNum, num_nodes=NumNodes, centralized=Centralized, 
                all_nodes=AllNodes, my_workers=MyWorkers, prev_throughput=PrevTh, current_round=CurrentRound, round_dict=RoundDict, 
                master=Master, myself=Myself, sum_throughput=SumThroughput, previous=Prev, current=Current}) ->
    case Centralized of
        false ->
            SumThroughput = 0,
            RemainNum = 1,
            NumNodes = 1,
            %{S1, B1, NewLength, PrevTh1} = get_new_length(PrevTh, Sml, Big, Mid, Throughput),
            {Current1, PrevTh1} = linear_new_length(Prev, Current, PrevTh, Throughput),
            lager:warning("Distribute: Previous length is ~w, current length is ~w", [Prev, Current1]),
            ets:insert(stat, {{auto_tune, CurrentRound}, Current1}),
            Workers = case MyWorkers of [] -> basho_bench_sup:workers();
                                        _ -> MyWorkers
                      end,
            lists:foreach(fun(Worker) -> gen_fsm:send_event(Worker, {specula_length, Current1}) end, Workers),
            case Current of
                Current1 ->
                    {next_state, gather_stat, State#state{previous=Prev, current_round=CurrentRound+1, current=Current1, prev_throughput=PrevTh1, my_workers=Workers}};
                _ ->
                    {next_state, gather_stat, State#state{previous=Current, current_round=CurrentRound+1, current=Current1, prev_throughput=PrevTh1, my_workers=Workers}}
            end;
        true ->
    	    lager:warning("Received ~w for round ~w, remain is ~w", [Throughput, Round, RemainNum]),
            case Master == Myself of
                true ->
                    case RemainNum of
                        1 ->
                            SumThroughput1 = SumThroughput + Throughput,
                            %{S1, B1, NewLength, PrevTh1} = get_new_length(PrevTh, Sml, Big, Mid, SumThroughput1),
                            {Current1, PrevTh1} = linear_new_length(Prev, Current, PrevTh, SumThroughput1),
                            lager:warning("Centralized: Previous length is ~w, current length is ~w", [Prev, Current1]),
                            lists:foreach(fun(Node) -> gen_fsm:send_event({global, Node}, {new_length, Current1}) end, AllNodes),
                            {Prev1, Current2} = case Current1 of Current -> {Prev, Current}; _ -> {Current, Current1} end, 
                            ets:insert(stat, {{auto_tune, CurrentRound}, Current1}),
                            case dict:find(CurrentRound+1, RoundDict) of
                            {ok, {Sum, Replied}} ->
                                {next_state, gather_stat, State#state{remain_num=NumNodes-Replied, sum_throughput=Sum,
                                    previous=Prev1, current=Current2, prev_throughput=PrevTh1, current_round=CurrentRound+1}};
                            error ->
                                {next_state, gather_stat, State#state{remain_num=NumNodes, sum_throughput=0,
					                previous=Prev1, current=Current2, prev_throughput=PrevTh1, current_round=CurrentRound+1}}
			                end;
                        _ ->
                            case Round > CurrentRound of
                                true ->
                                    RoundDict1 = dict:update(Round, fun({RSum, RCount}) -> {RSum+Throughput, RCount+1} end, {0,0}, RoundDict),
                                    {next_state, gather_stat, State#state{round_dict=RoundDict1}};
                                false ->
                                    Round = CurrentRound,
                                    {next_state, gather_stat, State#state{remain_num=RemainNum-1, sum_throughput=SumThroughput+Throughput}}
                            end
                    end;
                false ->
		            lager:warning("Sending to master ~w, current round is ~w", [Master, CurrentRound]),
                    gen_fsm:send_event({global, Master}, {throughput, CurrentRound, Throughput}),
                    {next_state, gather_stat, State#state{current_round=CurrentRound+1}}
            end
    end;

gather_stat({new_length, NewLength} , State=#state{my_workers=MyWorkers}) ->
    Workers = case MyWorkers of [] -> basho_bench_sup:workers();
                                _ -> MyWorkers
              end,
        lists:foreach(fun(Worker) -> gen_fsm:send_event(Worker, {specula_length, NewLength}) end, Workers),
    {next_state, gather_stat, State#state{my_workers=Workers}}.

handle_info(_Info, _StateName, StateData) ->
    {stop, badmsg, StateData}.

handle_event(_Event, _StateName, StateData) ->
    {stop, badmsg, StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop, badmsg, StateData}.

terminate(_Reason, _, _State) ->
    ok.

code_change(_OldVsn, _, State, _Extra) ->
    {ok, execute, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
linear_new_length(Prev, Current, Dict, Throughput) ->
    case Prev of
        -1 -> %% This is the first time!
            lager:warning("Prev is -1, Current is 0, throughput is ~w", [Throughput]),
            Current = 0,
            {1, dict:store(0, Throughput, Dict)};
        _ ->
            PrevTh = dict:fetch(Prev, Dict),
            lager:warning("Prev is ~w, prevth is ~w, curr is ~w, curr th is ~w", [PrevTh, Prev, Throughput, Current]),
            case Throughput > PrevTh of
                true ->
                    case Current > Prev of
                        true ->
                            {min(Current+1, ?BIG), dict:store(Current, Throughput, Dict)};
                        false -> 
                            {max(Current-1, ?SML), dict:store(Current, Throughput, Dict)}
                    end;
                false ->
                    case Current > Prev of
                        true ->
                            {max(Current-1, ?SML), dict:store(Current, Throughput, Dict)};
                        false -> 
                            {min(Current+1, ?BIG), dict:store(Current, Throughput, Dict)}
                    end
            end
    end.

binary_search(Dict, Small, Big, Mid, Throughput) ->
    case (Big == ?BIG) and (Small == ?SML) and (Mid == ?SML) of
        true -> % First time!
            D1 = dict:store(Small, Throughput, Dict),
            {Small, Big, (Big + Small) div 2, D1};
        false ->
            Dict1 = dict:store(Mid, Throughput, Dict),
            SmallTh = dict:fetch(Small, Dict),
            %io:format("Small th is ~w, th is ~w ~n", [SmallTh, Throughput]),
	    lager:warning("Current pos is ~w, th is ~w, small th is ~w", [Mid, Throughput, SmallTh]),
            case Throughput > SmallTh of
                true ->
                    S1 = Mid,
                    M1 = (S1 + Big) div 2,
                    case M1 of 
                        Mid -> {S1, Big, Big, Dict1};
                        _ -> {S1, Big, M1, Dict1}
                    end;
                false ->
                    B1 = Mid,
                    M1 = (Small + B1) div 2,
                    case M1 of
                        Mid -> {Small, B1, Small, Dict1};
                        _ -> {Small, B1, M1, Dict1}
                    end
            end
    end.

get_ip([], Prev) ->
    lists:reverse(Prev);
get_ip([C|T], Prev) ->
    D = [C],
    case D of
        "@" -> T;
        _ -> get_ip(T, [C|Prev])
    end.

tuner_name() ->
    list_to_atom( get_ip(atom_to_list(node()), [])  ++ "auto_tuner").
