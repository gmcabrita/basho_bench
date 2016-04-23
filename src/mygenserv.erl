-include("basho_bench.hrl").

-module(mygenserv).            
                               
-behaviour(gen_server).        
                               
-define(SERVER, ?MODULE).      
                               
-record(state, {count}).       
                               

%%-----------------------------------------------------------------------------
%% API Function Exports
%%-----------------------------------------------------------------------------

-export([                       
  start_link/0,                
  stop/0,                
  launchWorkersSup/1,
  launchWorkers/0
  ]).
  
%% ---------------------------------------------------------------------------
%% gen_server Function Exports
%% ---------------------------------------------------------------------------

-export([                      
  init/1,                      
  handle_call/3,               
  handle_cast/2,               
  handle_info/2,              
  terminate/2,                 
  code_change/3]).             

%% worker
-record(stateW, {id,
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

%% supervisor
-record(stateS, {workers,
				measurements}).

%% ---------------------------------------------------------------------------
%% API Function Definitions
%% ---------------------------------------------------------------------------

start_link() ->                
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).                     

stop() ->                      
    gen_server:cast(?SERVER, stop).                          
                               
                               
launchWorkersSup({SW, SD, SS}) ->
	io:fwrite("hello from mygenserv:launchWorkersSup before\n"),
	gen_server:call(?SERVER, {launchWorkersSup, SW, SD, SS}),
	io:fwrite("hello from mygenserv:launchWorkersSup after\n").
  
launchWorkers() ->
	io:fwrite("hello from mygenserv:launchWorkers before\n"),
	gen_server:call(?SERVER, {launchWorkers}),
	io:fwrite("hello from mygenserv:launchWorkers after\n").    
  
%set_config({SW, SD}) ->              
%	gen_server:call(?SERVER, {set_config, {SW, SD}}).
    
                           
%% ---------------------------------------------------------------------------
%% gen_server Function Definitions
%% ---------------------------------------------------------------------------

init([]) ->                    
    {ok, #state{count=0}}.  
                                  

handle_call({launchWorkersSup, SW, SD, SS}, _From, State) -> 
	io:fwrite("hello from mygenserv:handle_call launchWorkersSup 0 \n"),
	basho_bench_sup:start_link({SW, SD, SS}),
	io:fwrite("hello from mygenserv:handle_call launchWorkersSup 1\n"),
    {reply, ok, State}; 
    
handle_call({launchWorkers}, _From, State) -> 
	io:fwrite("hello from mygenserv:handle_call launchWorkers 0\n"),
	basho_bench_worker:run(basho_bench_sup:workers()),
	io:fwrite("hello from mygenserv:handle_call launchWorkers 1\n"),
	{reply, ok, State}.
    
handle_cast(stop, State) ->    
    {stop, normal, State};                         

handle_cast(say_hello, State) -> 
    io:format("Hello~n"),      
    {noreply, State}.                         

handle_info(Info, State) ->      
    error_logger:info_msg("~p~n", [Info]), 
    {noreply, State}.          

terminate(_Reason, _State) ->  
    error_logger:info_msg("terminating~n"), 
    ok.                        

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.               

