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
                 big,
                 sml,
                 mid,
                 num_nodes,
                 all_nodes,
                 remain_num,
                 sum_throughput = 0
               }).

-include("basho_bench.hrl").

%% ====================================================================
%% API
%% ====================================================================

start_link() ->
    lager:warning("AtomList is ~p", [atom_to_list(node())]),
    Name = tuner_name(), 
    lager:warning("Name is ~w!!", [Name]),
    lager:warning("After name!!"),
    gen_fsm:start_link({global, Name}, ?MODULE, [Name], []).

%% ====================================================================
%% gen_server callbacks
%% ====================================================================

init(Name) ->
    lager:warning("In init!!"),
    Myself = Name, 
    Centralized = basho_bench_config:get(centralized),
    Sml = ?SML,
    Big = ?BIG,
    Mid = Sml,
    PrevThroughput = lists:foldl(fun(N, D) ->
                dict:store(N, inf, D)
                end, dict:new(), lists:seq(Sml, Big)),
    case Centralized of
        false ->
            MyWorkers = basho_bench_sup:workers(),
            {ok, gather_stat, #state{ prev_throughput=PrevThroughput,
                            myself = Myself,
                            centralized = Centralized,
                            sml = Sml,
                            big = Big,
                            mid = Mid,
                            num_nodes=1,
                            remain_num=1,
                            my_workers = MyWorkers,
                            all_nodes = [],
                            sum_throughput = 0}};
        true ->
            AllNodes = basho_bench_config:get(all_nodes),
            AllTuners = lists:foreach(fun(Node) -> list_to_atom( get_ip(atom_to_list(Node), []) ++"auto_tuner") end,
                            AllNodes),
            NumNodes = length(AllNodes),
            [MasterNode|_] = AllNodes, 
            Master = list_to_atom( atom_to_list(MasterNode) ++ "auto_tuner"),
            MyWorkers = basho_bench_sup:workers(),
            {ok, gather_stat, #state{ prev_throughput=PrevThroughput,
                            myself = Myself,
                            master = Master,
                            my_workers=MyWorkers,
                            centralized = Centralized,
                            num_nodes=NumNodes,
                            sml = Sml,
                            big = Big,
                            mid = Mid,
                            all_nodes = AllTuners,
                            sum_throughput = 0}}
    end.

gather_stat({throughput, Throughput}, State=#state{remain_num=RemainNum, num_nodes=NumNodes, centralized=Centralized, 
                all_nodes=AllNodes, my_workers=MyWorkers, prev_throughput=PrevTh, 
                master=Master, myself=Myself, sum_throughput=SumThroughput, sml=Sml, big=Big, mid=Mid}) ->
    case Centralized of
        false ->
            SumThroughput = 0,
            RemainNum = 1,
            NumNodes = 1,
           {S1, B1, NewLength, PrevTh1} = get_new_length(PrevTh, Sml, Big, Mid, Throughput),
            lager:warning("Distribute: Previous length is ~w, current length is ~w", [Mid, NewLength]),
            Workers = case MyWorkers of [] -> basho_bench_sup:workers();
                                        _ -> MyWorkers
                      end,
            lists:foreach(fun(Worker) -> gen_fsm:send_event(Worker, {specula_length, NewLength}) end, Workers),
            {next_state, gather_stat, State#state{sml=S1, big=B1, mid=NewLength, prev_throughput=PrevTh1, my_workers=Workers}};
        true ->
            case Master == Myself of
                true ->
                    case RemainNum of
                        1 ->
                            SumThroughput1 = SumThroughput + Throughput,
                            {S1, B1, NewLength, PrevTh1} = get_new_length(PrevTh, Sml, Big, Mid, SumThroughput1),
                            lager:warning("Centralized: Previous length is ~w, current length is ~w", [Mid, NewLength]),
		    	    lager:warning("Sending to nodes ~w", [AllNodes]),
                            lists:foreach(fun(Node) -> gen_fsm:send_event({global, Node}, {new_length, NewLength}) end, AllNodes),
                            {next_state, gather_stat, State=#state{remain_num=NumNodes, sum_throughput=0,
                                sml=S1, big=B1, mid=NewLength, prev_throughput=PrevTh1}};
                        _ ->
                            {next_state, gather_stat, State=#state{remain_num=RemainNum-1, sum_throughput=SumThroughput+Throughput}}
                    end;
                false ->
		    lager:warning("Sending to master ~w", [Master]),
                    gen_fsm:send_event({global, Master}, {throughput, Throughput}),
                    {next_state, gather_stat, State}
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

get_new_length(Dict, Small, Big, Mid, Throughput) ->
    case (Big == ?BIG) and (Small == ?SML) and (Mid == ?SML) of
        true -> % First time!
            D1 = dict:store(Small, Throughput, Dict),
            {Small, Big, (Big + Small) div 2, D1};
        false ->
            Dict1 = dict:store(Mid, Throughput, Dict),
            SmallTh = dict:fetch(Small, Dict),
            %io:format("Small th is ~w, th is ~w ~n", [SmallTh, Throughput]),
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
