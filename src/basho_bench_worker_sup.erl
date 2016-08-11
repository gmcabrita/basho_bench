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
-module(basho_bench_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/1,
         workers/0,
         start_children/4,
         stop_child/1]).

%% Supervisor callbacks
-export([init/1]).

-include("basho_bench.hrl").

%% Helper macro for declaring children of supervisor
%-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
%% Change from permanent to transient, because children will be stoped in the end of benchmark

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, []).

workers() ->
    [Pid || {_Id, Pid, worker, [basho_bench_fsm_worker]} <- supervisor:which_children(?MODULE)].

stop_child(Id) ->
    ok = supervisor:terminate_child(?MODULE, Id),
    ok = supervisor:delete_child(?MODULE, Id).

start_children(Sender, Sup, Start, End) ->
    lists:foreach(fun(Id) ->
         supervisor:start_child(Sup, [Id])
            end, lists:seq(Start, End)),
    lager:warning("~w finished spawnning! Start is ~w, End is ~w", [self(), Start, End]),
    Sender ! done.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Worker = {basho_bench_fsm_worker,
              {basho_bench_fsm_worker, start_link, []},
               transient, 5000, worker, [basho_bench_fsm_worker]},
    {ok, {{simple_one_for_one, 100, 5}, [Worker]}}.

%init([Id, NumWorkers]) ->
    %% Get the number concurrent workers we're expecting and generate child
    %% specs for each

    %% intentionally left in to show where worker profiling start/stop calls go.
    %% eprof:start(),
    %% eprof:start_profiling([self()]),
%    Workers = worker_specs(NumWorkers*(Id-1)+1, NumWorkers*(Id-1)+NumWorkers, []),

%    {ok, {{one_for_one, 100, 5},
%        Workers 
%    }}.

%% ===================================================================
%% Internal functions
%% ===================================================================

%worker_specs(Start, Current, Acc) when Current < Start ->
%    Acc;
%worker_specs(Start, Current, Acc) ->
%    Id = list_to_atom(lists:concat(['basho_bench_worker_', Current])),
%    Spec = {Id, {basho_bench_fsm_worker, start_link, [Id, Current]},
%            permanent, 5000, worker, [basho_bench_fsm_worker]},
%    worker_specs(Start, Current-1, [Spec | Acc]).
