-module(myleader).

-behaviour(supervisor).

-include("basho_bench.hrl").

%% API
-export([start_link/0, start/0]).

%% Supervisor callbacks
-export([init/1]).

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

start_link() ->
	io:fwrite("hello from myleader:start_link\n"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    

start() ->
	io:format("hello from myleader:start 0\n"),
	{SW, SD, SS} = recup(),
	io:format("hello from myleader:start 1\n"),
%	ok = mygenserv:set_config({SW,SD}),
	ok = mygenserv:launchWorkersSup({SW, SD, SS}),
	io:fwrite("hello from leader:start 2\n"),
	ok = application:set_env(basho_bench_app, is_running, true),
	io:fwrite("hello from leader:start 3\n"),
	ok = basho_bench_stats:run(),
	io:fwrite("hello from leader:start 4\n"),
	ok = basho_bench_measurement:run(),
	io:fwrite("hello from leader:start 5\n"),
	ok = mygenserv:launchWorkers().

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	io:fwrite("hello from myleader:init\n"),

   {ok,                   % ok, supervisor here's what we want you to do
  {                       
    {                     % Global supervisor options
      one_for_one,        % - use the one-for-one restart strategy
      1000,               % - and allow a maximum of 1000 restarts
      3600                % - per hour for each child process
    },                     
    [                     % The list of child processes you should supervise
      {                   % We only have one
        mygenserv,     	  % - Register it under the name mygenserv
        {                 % - Here's how to find and start this child's code 
          mygenserv,   	  %   * the module is called mygenserv
          start_link,     %   * the function to invoke is called start_link
          []              %   * and here's the list of default parameters to use
        },                
        permanent,        % - child should run permantenly, restart on crash 
        2000,             % - give child 2 sec to clean up on system stop, then kill 
        worker,           % - FYI, this child is a worker, not a supervisor
        [mygenserv]    	  % - these are the modules the process uses  
      } 
    ]                     
  }                        
}. 

recup() ->
	io:fwrite("hello from myleader:recup\n"),
	{A1, A2, A3} =
        case basho_bench_config:get(rng_seed, {42, 23, 12}) of
            {Aa, Ab, Ac} -> {Aa, Ab, Ac};
            now -> now()
        end,
    RngSeed = {A1,A2,A3},

    %% Pull all config settings from environment
    Driver  = basho_bench_config:get(driver),
    Ops     = ops_tuple(),
    ShutdownOnError = basho_bench_config:get(shutdown_on_error, false),

    %% Finally, initialize key and value generation. We pass in our ID to the
    %% initialization to enable (optional) key/value space partitioning
    KeyGen = basho_bench_config:get(key_generator),
    ValGen = basho_bench_config:get(value_generator),
    Mode = basho_bench_config:get(mode),

    StateW = #stateW { keygen = KeyGen, valgen = ValGen,
                     driver = Driver,
                     shutdown_on_error = ShutdownOnError,
                     ops = Ops, ops_len = size(Ops),
                     rng_seed = RngSeed,
                     mode = Mode},



    IPs = basho_bench_config:get(antidote_pb_ips),
    PbPort = basho_bench_config:get(antidote_pb_port),
    Types  = basho_bench_config:get(antidote_types),
    SetSize = basho_bench_config:get(set_size),
    NumUpdates  = basho_bench_config:get(num_updates),
    NumReads = basho_bench_config:get(num_reads),
    NumPartitions = length(IPs),
    MeasureStaleness = basho_bench_config:get(staleness),

    StateD = #stateD {ips = IPs,
                      types = Types,
                      set_size = SetSize,
                      num_partitions = NumPartitions,
                      pb_port=PbPort,
                      num_reads=NumReads, 
                      num_updates=NumUpdates,
                      measure_staleness=MeasureStaleness},

    Workers = basho_bench_config:get(concurrent),
    MeasurementDriver = basho_bench_config:get(measurement_driver, undefined),

    StateS = #stateS {workers = Workers,
                      measurements = MeasurementDriver},

{StateW, StateD, StateS}.

%%
%% Expand operations list into tuple suitable for weighted, random draw
%%
ops_tuple() ->
	io:fwrite("hello from myleader:ops_tuple\n"),
    F =
        fun({OpTag, Count}) ->
                lists:duplicate(Count, {OpTag, OpTag});
           ({Label, OpTag, Count}) ->
                lists:duplicate(Count, {Label, OpTag})
        end,
    Ops = [F(X) || X <- basho_bench_config:get(operations, [])],
    list_to_tuple(lists:flatten(Ops)).


