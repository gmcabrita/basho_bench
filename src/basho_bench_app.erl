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
-module(basho_bench_app).

-behaviour(application).

%% API
-export([start/0,
         stop/0,
         is_running/0,
         halt_or_kill/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% API
%%===================================================================

start() ->
    %% Redirect all SASL logging into a text file
    case application:get_env(basho_bench,app_run_mode) of
       {ok, included} ->
          %%Make sure sasl and crypto is available
          true=lists:keymember(sasl,1,application:which_applications()),
          true=lists:keymember(crypto,1,application:which_applications()),
          
          %% Start up our application (this is the same as application:start(basho_bench, temporary))
            io:fwrite("hello from app:start temporary\n"),
          application:start(basho_bench);  
       NotInc when NotInc == {ok, standalone} orelse NotInc == undefined ->
          application:load(sasl),
          application:set_env(sasl, sasl_error_logger, {file, "log.sasl.txt"}),
          %% Make sure crypto is available
          ensure_started([sasl, crypto]),

          %% Start up our application -- mark it as permanent so that the node
          %% will be killed if we go down
          io:fwrite("hello from app:start permanent\n"),
          application:start(basho_bench, permanent)
    end.

stop() ->
    application:stop(basho_bench).

is_running() ->
    application:get_env(basho_bench_app, is_running) == {ok, true}.

halt_or_kill() ->
    %% If running standalone, halt and kill node.  Otherwise, just
    %% kill top supervisor.
    case application:get_env(basho_bench,app_run_mode) of
        {ok, included} ->
            exit(whereis(basho_bench_sup),kill);
        _ ->
            init:stop()
    end.

%% ===================================================================
%% Application callbacks
%%===================================================================

%start(_StartType, _StartArgs) -> 
%	{ok, Pid} = myleader:start_link(),
%		io:fwrite("hello from app:start 1\n"),
%	 ok = mygenserv:launchWorkersSup(),
%   		io:fwrite("hello from app:start 2\n"),
%    ok = application:set_env(basho_bench_app, is_running, true),
%   		io:fwrite("hello from app:start 3\n"),
%    ok = basho_bench_stats:run(),
%    	io:fwrite("hello from app:start 4\n"),
%    ok = basho_bench_measurement:run(),
%    	io:fwrite("hello from app:start 5\n"),
%    ok = mygenserv:launchWorkers(),
%    	io:fwrite("hello from app:start 6\n"),
%    {ok, Pid}.
     
start(_StartType, _StartArgs) ->  
	{ok, Pid} = myleader:start_link(),
	io:fwrite("hello from app:start 1\n"),
	ok = myleader:start(),
	%starts the leader as a process   
	%spawn(myleader, start),      
	io:fwrite("hello from app:start 2\n"), 
    {ok, Pid}.

          

stop(_State) ->
    %% intentionally left in to show where worker profiling start/stop calls go.
    %% eprof:stop_profiling(),
    %% eprof:analyze(total),
    %% eprof:log("bb.eprof"),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

ensure_started(Applications) when is_list(Applications) ->
  [ensure_started(Application) || Application <- Applications];

ensure_started(Application) ->
  case application:start(Application) of
    ok ->
      ok;
    {error, {already_started, Application}} ->
      ok;
    Error ->
      throw(Error)
  end.
