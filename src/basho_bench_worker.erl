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
-module(basho_bench_worker).

-behaviour(gen_server).

%% API
-export([start_link/2,
         run/1,
         cleanup/1,
         retry_until_not/3,
         stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, { id,
                 keygen,
                 valgen,
                 think_time,
                 driver,
                 driver_state,
                 shutdown_on_error,
                 ops,
                 todo_op,
                 cdf,
                 retry,
                 transition,
                 ops_len,
                 rng_seed,
                 parent_pid,
                 worker_pid,
                 sup_id}).

-include("basho_bench.hrl").

%% ====================================================================
%% API
%% ====================================================================

start_link(SupChild, Id) ->
    gen_server:start_link(?MODULE, [SupChild, Id], []).

run(Pids) ->
    [ok = gen_server:call(Pid, run) || Pid <- Pids],
    ok.

cleanup(Pids) ->
    %lager:info("Sending cleanup to ~w", [Pids]),
    [Pid ! {'CLEANUP', nothing} || Pid <- Pids],
    timer:sleep(3000),
    ok.

stop(Pids) ->
    [ok = gen_server:call(Pid, stop) || Pid <- Pids],
    ok.

%% ====================================================================
%% gen_server callbacks
%% ====================================================================

init([SupChild, Id]) ->
    %% Setup RNG seed for worker sub-process to use; incorporate the ID of
    %% the worker to ensure consistency in load-gen
    %%
    %% NOTE: If the worker process dies, this obviously introduces some entroy
    %% into the equation since you'd be restarting the RNG all over.
    %%
    %% The RNG_SEED is static by default for replicability of key size
    %% and value size generation between test runs.
    process_flag(trap_exit, true),
    {A1, A2, A3} =
        case basho_bench_config:get(rng_seed, {42, 23, 12}) of
            {Aa, Ab, Ac} -> {Aa, Ab, Ac};
            now -> now()
        end,

    RngSeed = {A1+Id, A2+Id, A3+Id},

    %% Pull all config settings from environment
    Driver  = basho_bench_config:get(driver),
    Ops     = ops_tuple(),
    ShutdownOnError = basho_bench_config:get(shutdown_on_error, false),
    Retry = case basho_bench_config:get(retry, false) of
                true -> true;
                _ -> false
            end,
   
    ThinkTime = case basho_bench_config:get(think_time, 0) of
                Time -> Time
            end,

    CDF = case basho_bench_config:get(cdf, false) of
                true -> Tab = ets:new(list_to_atom(integer_to_list(Id)), [public, set]),
                        {0, Tab};
                _ -> false
            end,
    {ToDoOp, Transition} = case basho_bench_config:get(transition, false) of
                true -> {{[], 1}, rubis_tool:load_transition()}; 
                _ -> {false, undef} 
            end,

    %L = dict:to_list(Transition),
    %lists:foreach(fun({Num, {_, TList}}) ->
    %                  AccV = lists:foldl(fun(V, Acc) -> Acc+V end, 0, TList),
    %                  lager:info("Num is ~w, Sum is ~w", [Num, AccV])
    %              end, L), 

    %lager:info("Transition is ~w", [dict:to_list(Transition)]),
    %% Finally, initialize key and value generation. We pass in our ID to the
    %% initialization to enable (optional) key/value space partitioning
    KeyGen = basho_bench_keygen:new(basho_bench_config:get(key_generator), Id),
    ValGen = basho_bench_valgen:new(basho_bench_config:get(value_generator), Id),

    State = #state { id = Id, keygen = KeyGen, valgen = ValGen,
                     driver = Driver, cdf=CDF,
                     shutdown_on_error = ShutdownOnError,
                     ops = Ops, ops_len = size(Ops),
                     rng_seed = RngSeed,
                     think_time = ThinkTime,
                     retry = Retry,
                     transition = Transition,
                     todo_op = ToDoOp,
                     parent_pid = self(),
                     sup_id = SupChild},

    %% Use a dedicated sub-process to do the actual work. The work loop may need
    %% to sleep or otherwise delay in a way that would be inappropriate and/or
    %% inefficient for a gen_server. Furthermore, we want the loop to be as
    %% tight as possible for peak load generation and avoid unnecessary polling
    %% of the message queue.
    %%
    %% Link the worker and the sub-process to ensure that if either exits, the
    %% other goes with it.
    WorkerPid = spawn_link(fun() -> worker_init(State) end),
    WorkerPid ! {init_driver, self()},
    receive
        driver_ready ->
            ok
    end,

    %% If the system is marked as running this is a restart; queue up the run
    %% message for this worker
    case basho_bench_app:is_running() of
        true ->
            ?WARN("Restarting crashed worker.\n", []),
            gen_server:cast(self(), run);
        false ->
            ok
    end,

    {ok, State#state { worker_pid = WorkerPid }}.

handle_call(run, _From, State) ->
    State#state.worker_pid ! run,
    {reply, ok, State}.

handle_cast(run, State) ->
    State#state.worker_pid ! run,
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State) ->
    case Reason of
        normal ->
            %% Clean shutdown of the worker; spawn a process to terminate this
            %% process via the supervisor API and make sure it doesn't restart.
            %lager:warning("CDF is ~w", [State#state.cdf]),
            spawn(fun() -> stop_worker(State#state.sup_id) end),
	    (catch (State#state.driver):terminate({'EXIT', Reason}, State#state.driver_state)),
            {noreply, State};
	cleanup ->
	    lager:info("Cleaning up, driver is ~w, state is ~w", [State#state.driver, State#state.driver_state]),
	    (catch (State#state.driver):terminate({'EXIT', Reason}, State#state.driver_state)),
            {noreply, State};
        _ ->
            ?ERROR("Worker ~p exited with ~p~n", [Pid, Reason]),
	    (catch (State#state.driver):terminate({'EXIT', Reason}, State#state.driver_state)),
            %% Worker process exited for some other reason; stop this process
            %% as well so that everything gets restarted by the sup
            {stop, normal, State}
    end;

handle_info({'CLEANUP', nothing}, State=#state{ worker_pid = WorkerPid }) ->
    case WorkerPid of
	undefined ->
	    lager:info("Cleaning up, driver is ~w, state is ~w", [State#state.driver, State]),
	    (catch (State#state.driver):terminate(haha, State#state.driver_state)),
	    {noreply, State};
	 _ ->
	    WorkerPid ! {'CLEANUP', nothing},
	    {noreply, State}
    end.

terminate(_Reason, State) ->
    case State#state.cdf of false -> ok;
                _ ->
                    {_, Tab} = State#state.cdf,
                    List = ets:tab2list(Tab),
                    FileName = integer_to_list(State#state.id) ++ "latency",
                    {ok, File} = file:open(FileName, [raw, binary, write]),
                    lists:foreach(fun({_, Lat}) ->
                                  %lager:info("Lat is ~p", [Lat]),
                                  file:write(File,  io_lib:format("~w\n", [Lat]))
                                  end, List),
                    file:close(File)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% ====================================================================
%% Internal functions
%% ====================================================================

%%
%% Stop a worker process via the supervisor and terminate the app
%% if there are no workers remaining
%%
%% WARNING: Must run from a process other than the worker!
%%
stop_worker(SupChild) ->
    ok = basho_bench_sup:stop_child(SupChild),
    case basho_bench_sup:workers() of
        [] ->
            %% No more workers -- stop the system
	        lager:info("Worker trying to stop app"),
            basho_bench_app:stop();
        _ ->
            ok
    end.

%%
%% Expand operations list into tuple suitable for weighted, random draw
%%
ops_tuple() ->
    F =
        fun({OpTag, Count}) ->
                lists:duplicate(Count, {OpTag, OpTag});
           ({Label, OpTag, Count}) ->
                lists:duplicate(Count, {Label, OpTag})
        end,
    Ops = [F(X) || X <- basho_bench_config:get(operations, [])],
    list_to_tuple(lists:flatten(Ops)).


worker_init(State) ->
    %% Trap exits from linked parent process; use this to ensure the driver
    %% gets a chance to cleanup
    process_flag(trap_exit, true),
    random:seed(State#state.rng_seed),
    worker_idle_loop(State).

worker_idle_loop(State) ->
    Driver = State#state.driver,
    receive
        {init_driver, Caller} ->
            %% Spin up the driver implementation
            case catch(Driver:new(State#state.id)) of
                {ok, DriverState} ->
                    Caller ! driver_ready,
                    ok;
                Error ->
                    DriverState = undefined, % Make erlc happy
                    ?FAIL_MSG("Failed to initialize driver ~p: ~p\n", [Driver, Error])
            end,
            worker_idle_loop(State#state { driver_state = DriverState });
        run ->
            case basho_bench_config:get(mode) of
                max ->
                    ?INFO("Starting max worker: ~p\n", [self()]),
                    max_worker_run_loop(State);
                {rate, max} ->
                    ?INFO("Starting max worker: ~p\n", [self()]),
                    max_worker_run_loop(State);
                {rate, Rate} ->
                    %% Calculate mean interarrival time in in milliseconds. A
                    %% fixed rate worker can generate (at max) only 1k req/sec.
                    MeanArrival = 1000 / Rate,
                    ?INFO("Starting ~w ms/req fixed rate worker: ~p\n", [MeanArrival, self()]),
                    rate_worker_run_loop(State, 1 / MeanArrival)
            end
    end.

worker_next_op2(State, OpTag) ->
   catch (State#state.driver):run(OpTag, State#state.keygen, State#state.valgen,
                                  State#state.driver_state).
worker_next_op(State) ->
    ToDo = case State#state.retry of 
                true ->
                    case State#state.todo_op of
                        false -> 
                            timer:sleep(State#state.think_time),
                            element(random:uniform(State#state.ops_len), State#state.ops);
                        Op ->  Op
                    end;
                false ->
                    ST = State#state.transition,
                    timer:sleep(State#state.think_time),
                    case ST of
                        undef ->
                            element(random:uniform(State#state.ops_len), State#state.ops);
                        _ ->
                            %% Stick to register user
                            State#state.todo_op
                    end
            end,
    {PreviousOps, OpTag} = ToDo,
    TranslatedOp = rubis_tool:translate_op(OpTag),
    %lager:warning("Current ToDo is ~w", [ToDo]),
    %lager:warning("Current Op is ~w", [TranslatedOp]),
    Start = os:timestamp(),
    Result = worker_next_op2(State, TranslatedOp),
    ElapsedUs = erlang:max(0, timer:now_diff(os:timestamp(), Start)),
    case Result of
        {prev_state, DriverState} ->
            case PreviousOps of
                [] -> {ok, State#state {driver_state = DriverState, todo_op={[], 1}}};
                [H|T] ->
                    {ok, State#state {driver_state = DriverState, todo_op={T, H}}}
            end;
        {Res, DriverState} when Res == ok orelse element(1, Res) == ok ->
            basho_bench_stats:op_complete({TranslatedOp, TranslatedOp}, Res, ElapsedUs),
            T = State#state.transition,
            case T of
                undef -> 
                    case State#state.cdf of false -> {ok, State#state { driver_state = DriverState, todo_op=false}};
                                {Count, Table} -> 
                                                    ets:insert(Table, {Count+1, ElapsedUs}),
                                                    {ok, State#state{driver_state = DriverState, todo_op=false, cdf={Count+1, Table}}}
                    end;
                _ ->
                    {PreviousStates, CurrentState} = State#state.todo_op,
                    NextToDo = rubis_tool:get_next_state(PreviousStates, T, CurrentState), 
                    timer:sleep(State#state.think_time),
                    {ok, State#state { driver_state = DriverState, todo_op=NextToDo}}
            end;
        {Res, DriverState} when Res == silent orelse element(1, Res) == silent ->
            {ok, State#state { driver_state = DriverState, todo_op=false}};
        {error, Reason, DriverState} ->
            %% Driver encountered a recoverable error
            basho_bench_stats:op_complete({TranslatedOp, TranslatedOp}, {error, Reason}, ElapsedUs),
            State#state.shutdown_on_error andalso
                erlang:send_after(500, basho_bench,
                                  {shutdown, "Shutdown on errors requested", 1}),
            {ok, State#state { driver_state = DriverState, todo_op=ToDo}};

        {'EXIT', Reason} ->
            %% Driver crashed, generate a crash error and terminate. This will take down
            %% the corresponding worker which will get restarted by the appropriate supervisor.
            basho_bench_stats:op_complete(ToDo, {error, crash}, ElapsedUs),

            %% Give the driver a chance to cleanup
            (catch (State#state.driver):terminate({'EXIT', Reason}, State#state.driver_state)),

            ?DEBUG("Driver ~p crashed: ~p\n", [State#state.driver, Reason]),
            case State#state.shutdown_on_error of
                true ->
                    %% Yes, I know this is weird, but currently this
                    %% is how you tell Basho Bench to return a
                    %% non-zero exit status.  Ideally this would all
                    %% be done in the `handle_info' callback where it
                    %% would check `Reason' and `shutdown_on_error'.
                    %% Then I wouldn't have to return a bullshit "ok"
                    %% here.
                    erlang:send_after(500, basho_bench,
                                      {shutdown, "Shutdown on errors requested", 2}),
                    {ok, State};
                false ->
                    crash
            end;

        {stop, Reason} ->
            %% Driver (or something within it) has requested that this worker
            %% terminate cleanly.
            ?INFO("Driver ~p (~p) has requested stop: ~p\n", [State#state.driver, self(), Reason]),

            %% Give the driver a chance to cleanup
            (catch (State#state.driver):terminate(normal, State#state.driver_state)),

            normal
    end.

retry_until_not(PreviousStates, T, CurrentState) ->
    NextToDo = rubis_tool:get_next_state(PreviousStates, T, CurrentState), 
    case NextToDo of
        {_, 3} ->
            retry_until_not(PreviousStates, T, CurrentState);
        {_, 15} ->
            retry_until_not(PreviousStates, T, CurrentState);
        {_, 18} ->
            retry_until_not(PreviousStates, T, CurrentState);
        {_, 21} ->
            retry_until_not(PreviousStates, T, CurrentState);
        %{_, 25} ->
        %    retry_until_not(PreviousStates, T, CurrentState);
        %{_, 27} ->
        %    retry_until_not(PreviousStates, T, CurrentState);
        {_, _} ->
            NextToDo
    end.
    
    

needs_shutdown(State) ->
    Parent = State#state.parent_pid,
    receive
        {'EXIT', Pid, _Reason} ->
            case Pid of
                Parent ->
                    %% Give the driver a chance to cleanup
		    lager:info("Trying to shut down because of message"),
                    (catch (State#state.driver):terminate(normal,
                                                          State#state.driver_state)),
                    true;
                _Else ->
                    %% catch this so that selective recieve doesn't kill us when running
                    %% the riakclient_driver
		    lager:info("Still trying to shut down because of message"),
                    (catch (State#state.driver):terminate(normal,
                                                          State#state.driver_state)),
                    false
            end;
	{'CLEANUP', nothing} ->
	    (catch (State#state.driver):terminate(normal,
						  State#state.driver_state)),
	    true
    after 0 ->
            false
    end.


max_worker_run_loop(State) ->
    case worker_next_op(State) of
        {ok, State2} ->
            case needs_shutdown(State2) of
                true ->
		    %lager:warning("*********Terminating worker now**********"),
                    ok;
                false ->
                    max_worker_run_loop(State2)
            end;
        ExitReason ->
            lager:info("CDF is ~w", [State#state.cdf]),
            exit(ExitReason)
    end.

rate_worker_run_loop(State, Lambda) ->
    %% Delay between runs using exponentially distributed delays to mimic
    %% queue.
    timer:sleep(trunc(basho_bench_stats:exponential(Lambda))),
    case worker_next_op(State) of
        {ok, State2} ->
            case needs_shutdown(State2) of
                true ->
                    ok;
                false ->
                    rate_worker_run_loop(State2, Lambda)
            end;
        ExitReason ->
            exit(ExitReason)
    end.
