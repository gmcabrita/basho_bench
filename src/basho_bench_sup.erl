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
-module(basho_bench_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         workers/0,
         start_children/1,
         stop_child/1]).

%% Supervisor callbacks
-export([init/1]).

-include("basho_bench.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
%% Change from permanent to transient, because children will be stoped in the end of benchmark
-define(NUM_WORKER_SUP, 4).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

workers() ->
    Sups = generate_sup_specs(?NUM_WORKER_SUP),
    AllChildren = lists:foldl(fun(Sup, Acc) -> [supervisor:which_children(Sup)|Acc] end, [], Sups),
    [Pid || {_Id, Pid, worker, [basho_bench_fsm_worker]} <- AllChildren].

stop_child(Id) ->
    ok = supervisor:terminate_child(?MODULE, Id),
    ok = supervisor:delete_child(?MODULE, Id).

start_children(Total) ->
    case ?NUM_WORKER_SUP > Total of
        true ->
            basho_bench_worker_sup:start_children('basho_bench_worker_sup_1', 1, Total); 
        false ->
            Avg = Total div ?NUM_WORKER_SUP,
            lists:foldl(fun(Sup, Id) ->
                    {Start, End} = case Id of ?NUM_WORKER_SUP -> {(Id-1)*Avg+1, Total};
                                                _ -> {(Id-1)*Avg+1, Id*Avg}
                                            
                                   end,
                    Self = self(),
                    spawn(fun() -> basho_bench_worker_sup:start_children(Self, Sup, Start, End) end),
                    case Id of 1 -> timer:sleep(500); _ -> ok end,
                    Id+1
                end, 1, generate_sup_specs(?NUM_WORKER_SUP)), 
            lists:foreach(fun(_) ->  receive  done ->  ok  end end, lists:seq(1, ?NUM_WORKER_SUP))
    end.


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %% Get the number concurrent workers we're expecting and generate child
    %% specs for each

    %% intentionally left in to show where worker profiling start/stop calls go.
    %% eprof:start(),
    %% eprof:start_profiling([self()]),
    WorkerSups = get_specs(?NUM_WORKER_SUP),
    MeasurementDriver =
        case basho_bench_config:get(measurement_driver, undefined) of
            undefined -> [];
            _Driver -> [?CHILD(basho_bench_measurement, worker)]
        end,

    {ok, {{one_for_one, 100, 5},
        [?CHILD(basho_bench_stats, worker)] ++
        WorkerSups ++
        MeasurementDriver
    }}.

%% ===================================================================
%% Internal functions
%% ===================================================================

generate_sup_specs(NumSup) ->
    lists:foldl(fun(Id, Acc) ->
            [list_to_atom(lists:concat(['basho_bench_worker_sup_', Id]))|Acc]
            end, [], lists:reverse(lists:seq(1, NumSup))).

get_specs(NumSup) ->
    %case NumSup > Total of
    %    true ->
    %        Id = list_to_atom(lists:concat(['basho_bench_worker_sup_', 1])),
    %        FirstOne = {Id, {basho_bench_worker_sup, start_link, [1, Total]},
    %                        permanent, 5000, supervisor, [basho_bench_worker_sup]},
    %        lists:foldl(fun(SupId, Acc) -> SupName = list_to_atom(lists:concat(['basho_bench_worker_sup_', SupId])),
    %                                Acc++[{SupName, {basho_bench_worker_sup, start_link, [SupId, 0]},
    %                                    permanent, 5000, supervisor, [basho_bench_worker_sup]}]
    %                            end, [FirstOne], lists:seq(2, NumSup));
    %    false ->
    Result = lists:foldl(fun(Index, Acc) ->
         Id = list_to_atom(lists:concat(['basho_bench_worker_sup_', Index])),
        [{Id, {basho_bench_worker_sup, start_link, [Id]},
            permanent, 5000, supervisor, [basho_bench_worker_sup]}|Acc]
            end, [], lists:seq(1, NumSup)),
    lists:reverse(Result).
    %end.

%worker_specs(NumSup, NumSup, Total, Acc) ->
%    Id = list_to_atom(lists:concat(['basho_bench_worker_sup_', NumSup])),
%    Spec = {Id, {basho_bench_worker_sup, start_link, [NumSup, (Total div NumSup + Total rem NumSup)]},
%            permanent, 5000, supervisor, [basho_bench_worker_sup]},
%    lists:reverse([Spec | Acc]);
%worker_specs(Count, NumSup, Total, Acc) ->
%    Id = list_to_atom(lists:concat(['basho_bench_worker_sup_', Count])),
%    Spec = {Id, {basho_bench_worker_sup, start_link, [Count, Total div NumSup]},
%            permanent, 5000, supervisor, [basho_bench_worker_sup]},
%    worker_specs(Count+1, NumSup, Total, [Spec | Acc]).
