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
-export([start_link/1,
         workers/0,
         stop_child/1]).

%% Supervisor callbacks
-export([init/1]).

-include("basho_bench.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% worker
-record(stateW, { id,
                 keygen,
                 valgen,
                 driver,
                 driver_state,
                 shutdown_on_error,
                 ops,
                 ops_len,
                 rng_seed,
                 parent_pid,
                 worker_pid,
                 sup_id,
                 mode}).

%% driver
-record(stateD, {ips,
                types,
                worker_id,
                time,
                type_dict,
                pb_pid,
                num_partitions,
                set_size,
                commit_time,
                num_reads,
                num_updates,
                pb_port,
                target_node,
                measure_staleness}).

%% sup
-record(stateS, { workers,
                  measurements}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link({SW, SD, SS}) ->
	io:fwrite("hello from sup:start_link\n"),
    supervisor:start_link({global, ?MODULE}, ?MODULE, [{SW, SD, SS}]).

workers() ->
	io:fwrite("hello from sup:workers before\n"),
	[Pid || {_Id, Pid, worker, [basho_bench_worker]} <- supervisor:which_children(?MODULE)].

stop_child(Id) ->
    ok = supervisor:terminate_child(?MODULE, Id),
    ok = supervisor:delete_child(?MODULE, Id).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([{SW, SD, SS}]) ->
	io:fwrite("hello from sup:init\n"),
    %% Get the number concurrent workers we're expecting and generate child
    %% specs for each

    %% intentionally left in to show where worker profiling start/stop calls go.
    %% eprof:start(),
    %% eprof:start_profiling([self()]),
    Workers = worker_specs(SS#stateS.workers, [],{SW, SD}),
    MeasurementDriver =
        case SS#stateS.measurements of
            undefined -> [];
            _Driver -> [?CHILD(basho_bench_measurement, worker)]
        end,
        io:format("sup: init() end\n"),
    {ok, {{one_for_one, 5, 10},
        [?CHILD(basho_bench_stats, worker)] ++
        Workers ++
        MeasurementDriver
    }}.

%% ===================================================================
%% Internal functions
%% ===================================================================

worker_specs(0, Acc, {SW,SD}) ->
    Acc;
worker_specs(Count, Acc, {SW, SD}) ->
	io:fwrite("hello from sup:worker_specs\n"),
    Id = list_to_atom(lists:concat(['basho_bench_worker_', Count])),
    Spec = {Id, {basho_bench_worker, start_link, [Id, Count, {SW, SD}]},
            permanent, 5000, worker, [basho_bench_worker]},
    worker_specs(Count-1, [Spec | Acc], {SW, SD}).
