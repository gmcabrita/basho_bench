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
-define(DELTA, 100000).

%% API
-export([start_link/2,
         run/1,
         cleanup/1,
         write_to_file/1,
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
                 %%
                 last_update_cnt,
                 update_seq,
                 read_seq,
                 op_type,
                 specula_txs,
                 read_txs,
                 msg_id,
                 final_cdf,
                 specula_cdf, 
                 do_specula,
                 store_cdf,
                 %op_list,
                 %cdf,
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

    {ToDoOp, Transition} = case basho_bench_config:get(transition, false) of
                true -> LoadTransition = rubis_tool:load_transition(), 
                     {{[], 1}, LoadTransition}; 
                _ -> {Info, FirstOpTag} = element(random:uniform(size(Ops)), Ops), 
                     {{Info, FirstOpTag}, undef}
            end,

    %lager:info("Transition is ~w", [dict:to_list(Transition)]),
    %% Finally, initialize key and value generation. We pass in our ID to the
    %% initialization to enable (optional) key/value space partitioning
    KeyGen = basho_bench_keygen:new(basho_bench_config:get(key_generator), Id),
    ValGen = basho_bench_valgen:new(basho_bench_config:get(value_generator), Id),

    State = #state { id = Id, keygen = KeyGen, valgen = ValGen,
                     driver = Driver, %cdf=CDF,
                     shutdown_on_error = ShutdownOnError,
                     ops = Ops, ops_len = size(Ops),
                     rng_seed = RngSeed,
                     think_time = ThinkTime,
                     do_specula = basho_bench_config:get(do_specula, false),
                     op_type = get_op_type(ToDoOp),
                     retry = Retry,
                     specula_txs=[],
                     read_txs=[],
                     update_seq=0,
                     read_seq=0,
                     store_cdf = {1, now(), basho_bench_config:get(store_to_table, 5000)*1000},
                     last_update_cnt=0,
                     msg_id=0,
                     specula_cdf=[],
                     final_cdf=[],
                     transition = Transition,
                     todo_op = ToDoOp,
                     parent_pid = self(),
                     sup_id = SupChild},

    {_, OpName} = ToDoOp,

    State1 = case get_op_type(ToDoOp) of
                update -> State#state{specula_txs=[{1, OpName, now(), ignore}], update_seq=1, last_update_cnt=1}; 
                read -> State#state{read_txs=[{1, OpName}], read_seq=1}
             end,

    %% Use a dedicated sub-process to do the actual work. The work loop may need
    %% to sleep or otherwise delay in a way that would be inappropriate and/or
    %% inefficient for a gen_server. Furthermore, we want the loop to be as
    %% tight as possible for peak load generation and avoid unnecessary polling
    %% of the message queue.
    %%
    %% Link the worker and the sub-process to ensure that if either exits, the
    %% other goes with it.
    WorkerPid = spawn_link(fun() -> worker_init(State1) end),
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

terminate(_Reason, _State) ->
    %case State#state.cdf of false -> ok;
    %            _ ->
    %                {_, Tab} = State#state.cdf,
    %                List = ets:tab2list(Tab),
    %                FileName = integer_to_list(State#state.id) ++ "latency",
    %                {ok, File} = file:open(FileName, [raw, binary, write]),
    %                lists:foreach(fun({_, Lat}) ->
    %                              %lager:info("Lat is ~p", [Lat]),
    %                              file:write(File,  io_lib:format("~w\n", [Lat]))
    %                              end, List),
    %                file:close(File)
    %end,
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

worker_next_op2(State, OpTag, update) ->
   catch (State#state.driver):run(OpTag, State#state.update_seq, State#state.msg_id, 
                                  State#state.driver_state);
worker_next_op2(State, OpTag, read) ->
   catch (State#state.driver):run(OpTag, State#state.read_seq, State#state.msg_id, 
                                  State#state.driver_state).
worker_next_op(State) ->
    Transition = State#state.transition,
    ToDo = State#state.todo_op,
    ThinkTime = State#state.think_time,
    {PreviousOps, OpTag} = ToDo,
    TranslatedOp = rubis_tool:translate_op(OpTag),
    Start = os:timestamp(),
    FinalCdf0 = State#state.final_cdf,
    SpeculaCdf0 = State#state.specula_cdf,
    ReadTxs = State#state.read_txs,
    SpeculaTxs = State#state.specula_txs,
    LastUpdateCnt = State#state.last_update_cnt,
    CurrentOpType = State#state.op_type,
    UpdateSeq = State#state.update_seq,
    ReadSeq = State#state.read_seq,
    {Cnt, ExprStart, Period} = State#state.store_cdf,
    MsgId = State#state.msg_id,
    %case CurrentOpType of
    %    update ->%lager:warning("Going to try op ~w, update seq is ~w, Specula txs are ~w, Specula Reads are ~w", [TranslatedOp, UpdateSeq, SpeculaTxs, ReadTxs]);
    %    read -> ok %lager:warning("Going to try op ~w, read seq is ~w, Specula txs are ~w", [TranslatedOp, ReadSeq, SpeculaTxs])
    %end,
    Result = worker_next_op2(State, TranslatedOp, CurrentOpType),
    Now = os:timestamp(),
    ElapsedUs = erlang:max(0, timer:now_diff(Now, Start)),
    TimerDiff = timer:now_diff(Now, ExprStart),
    {FinalCdf, SpeculaCdf, StoreCdf} 
            = case (TimerDiff > Period*Cnt) or (Period*Cnt-TimerDiff < ?DELTA) of
                 true -> ets:insert(final_cdf, {{Cnt, State#state.id}, FinalCdf0}), 
                         ets:insert(percv_cdf, {{Cnt, State#state.id}, SpeculaCdf0}),
                         {[], [], {Cnt+1, ExprStart, Period}};
                false -> {FinalCdf0, SpeculaCdf0, {Cnt, ExprStart, Period}}
              end,

    %lager:info("Result is ~p", [Result]),     
    case Result of
        {prev_state, DriverState} ->
            %case CurrentOpType of  update ->%lager:warning("Op ~p previous state", [UpdateSeq]); read -> ok end,
            case PreviousOps of
                [] ->
                    case ThinkTime of rubis -> timer:sleep(rubis_tool:get_think_time({1,1}, Transition));
                                        _ -> timer:sleep(ThinkTime)
                    end, 
                    {ok, State#state {driver_state = DriverState, todo_op={[], 1}, store_cdf=StoreCdf}};
                [H|T] ->
                    case ThinkTime of rubis -> timer:sleep(rubis_tool:get_think_time({H,H}, Transition));
                                        _ -> timer:sleep(ThinkTime)
                    end,
                    {ok, State#state {driver_state = DriverState, todo_op={T, H}, store_cdf=StoreCdf}}
            end;
        %%% A no-op has finished
        {Res, DriverState} when Res == ok orelse element(1, Res) == ok ->
            %case CurrentOpType of  update ->%lager:warning("Op ~p no-op committed", [UpdateSeq]); read -> ok end,
            %basho_bench_stats:op_complete({TranslatedOp, TranslatedOp}, ok, ElapsedUs),

            ReadTxs1 = finalize_reads([ReadSeq], ReadTxs, [], ok),
            NextOp = case Transition of
                        undef ->
                            element(random:uniform(State#state.ops_len), State#state.ops);
                        _ ->
                            {PreviousStates, CurrentOpName} = ToDo,
                            rubis_tool:get_next_state(PreviousStates, Transition, CurrentOpName)
                    end,
            {_, NextOpName} = NextOp,
            TransNextOp = rubis_tool:translate_op(NextOpName),

            OpThinkTime = op_think_time(ToDo, NextOp, ThinkTime, Transition),
            case deal_messages(0, FinalCdf, SpeculaCdf, SpeculaTxs, ReadTxs1, MsgId) of
                {FinalCdf2, SpeculaCdf2, SpeculaTxs3, ReadTxs3, NewMsgId, ignore, ignore} ->
                    case get_op_type(TransNextOp) of
                        update ->
                            NextUpdateSeq = UpdateSeq + 1,
                            timer:sleep(OpThinkTime),
                            SpeculaTxs4 = SpeculaTxs3 ++ [{NextUpdateSeq, TransNextOp, os:timestamp(), ignore}],
                            {ok, State#state{driver_state=DriverState, todo_op=NextOp, update_seq=NextUpdateSeq, last_update_cnt=NextUpdateSeq, store_cdf=StoreCdf, msg_id=NewMsgId,
                                specula_txs=SpeculaTxs4, read_txs=ReadTxs3, specula_cdf=SpeculaCdf2, final_cdf=FinalCdf2, op_type=update}};
                        read ->
                            NextReadSeq = ReadSeq + 1,
                            timer:sleep(OpThinkTime),
                            ReadTxs4 = ReadTxs3 ++ [{NextReadSeq, TransNextOp}],
                            {ok, State#state{driver_state=DriverState, todo_op=NextOp, read_seq=NextReadSeq, msg_id=NewMsgId,
                                  specula_txs=SpeculaTxs3, read_txs=ReadTxs4, specula_cdf=SpeculaCdf2, final_cdf=FinalCdf2
                                , store_cdf=StoreCdf, op_type=read}}
                    end;
                {FinalCdf2, SpeculaCdf2, SpeculaTxs3, ReadTxs3, NewMsgId, RetryOpSeq, OpName1} ->
                    {ok, State#state{driver_state=DriverState, todo_op={PreviousOps, OpName1}, update_seq=RetryOpSeq, store_cdf=StoreCdf,
                            specula_txs=SpeculaTxs3, read_txs=ReadTxs3, specula_cdf=SpeculaCdf2, final_cdf=FinalCdf2, msg_id=NewMsgId, op_type=update}}
            end;        
        %% Committed! Means all previous update txns are committed. So just start a new txn
        {Res, {AbortedReads, FinalCommitUpdates, FinalCommitReads}, DriverState} when Res == ok orelse element(1, Res) == ok ->
            %case CurrentOpType of  update ->%lager:warning("Op ~p committed", [UpdateSeq]); read -> ok end,
            %lager:warning("Op ~w finished, op type is ~w, specula txs are ~w", [OpTag, CurrentOpType, SpeculaTxs]),
            ReadTxs1 = finalize_reads(FinalCommitReads, ReadTxs, [], ok),
            ReadTxs2 = finalize_reads(AbortedReads, ReadTxs1, [], {error, abort}),
            %case FinalCommitReads of [] -> ok;
            %                         _ ->%lager:warning("FinalCommReads are ~w, Specula Txs is ~w, ReadTxs are ~w", [FinalCommitReads, SpeculaTxs, ReadTxs])
            %end,
            {FinalCdf1, SpeculaCdf1, SpeculaTxs1} = commit_updates(FinalCdf, SpeculaCdf, FinalCommitUpdates, SpeculaTxs, []),
            CurrentOpType = update,
            {FinalCdf2, SpeculaCdf2, []} = commit_updates(FinalCdf1, SpeculaCdf1, [{UpdateSeq, Now}], SpeculaTxs1, []),

            NextOp = case Transition of
                        undef ->
                            element(random:uniform(State#state.ops_len), State#state.ops);
                        _ ->
                            {PreviousStates, CurrentOpName} = ToDo,
                            rubis_tool:get_next_state(PreviousStates, Transition, CurrentOpName)
                    end,
            {_, NextOpName} = NextOp,
            OpThinkTime = op_think_time(ToDo, NextOp, ThinkTime, Transition),
            timer:sleep(OpThinkTime),
            case get_op_type(rubis_tool:translate_op(NextOpName)) of 
                update ->
                    NextUpdateSeq = UpdateSeq +1,
                    SpeculaTxs4 = [{NextUpdateSeq, NextOpName, os:timestamp(), ignore}], 
                    {ok, State#state { driver_state = DriverState, todo_op=NextOp, final_cdf=FinalCdf2, update_seq=NextUpdateSeq, 
                        specula_cdf=SpeculaCdf2, specula_txs=SpeculaTxs4, read_txs=ReadTxs2, last_update_cnt=NextUpdateSeq
                        , store_cdf=StoreCdf, op_type=update}};
                read ->
                    NextReadSeq = ReadSeq +1,
                    ReadTxs3 = [{NextReadSeq, NextOpName}|ReadTxs2], 
                    {ok, State#state { driver_state = DriverState, todo_op=NextOp, read_seq=NextReadSeq, read_txs=ReadTxs3, 
                        specula_txs=[], specula_cdf=SpeculaCdf2, final_cdf=FinalCdf2, store_cdf=StoreCdf, op_type=read}}
            end;
        {Res, DriverState} when Res == silent orelse element(1, Res) == silent ->
            {ok, State#state { driver_state = DriverState, todo_op=false}};
        %% Report final committed and final aborted.
        %% Wait for msg. Either retrying some ops if final abort msg is received or continue 
        {specula_commit, {AbortedReads, FinalCommitUpdates, FinalCommitReads}, DriverState} ->
            %case CurrentOpType of  update ->%lager:warning("Op ~p specula-committed", [UpdateSeq]); read -> ok end,
            %lager:warning("Finall commit updates are ~w, FinallComm Reads are ~w", [FinalCommitUpdates, FinalCommitReads]),
            ReadTxs1 = finalize_reads(FinalCommitReads, ReadTxs, [], ok),
            ReadTxs2 = finalize_reads(AbortedReads, ReadTxs1, [], {error, specula_abort}),
            %case FinalCommitReads of [] -> ok;
            %                            _ ->%lager:warning("FinalRead is ~w, Specula Txs is ~w", [FinalCommitReads, SpeculaTxs])
            %end,
             %lager:warning("Txns are ~p ~p", [FinalCommitUpdates, SpeculaTxs]),
            %case FinalCommitUpdates of [] -> ok; _ -> %lager:warning("FinalComm is ~w", [FinalCommitUpdates]) end,
            {FinalCdf1, SpeculaCdf1, SpeculaTxs1} = commit_updates(FinalCdf, SpeculaCdf, FinalCommitUpdates, SpeculaTxs, []),
            %% If I am update: add my specula-commit time to the list
            %% If I am read: add my txid to the list
            SpeculaTxs2 = case CurrentOpType of read -> SpeculaTxs1; 
                                            update -> add_sctime_to_list(SpeculaTxs1, UpdateSeq, Now)
                                      end,
            %% Decide new operations to perform
            case LastUpdateCnt == UpdateSeq of
                true -> %% Already performing the last operation, can choose new operation freely 
                    NextOp = case Transition of
                          undef ->
                              element(random:uniform(State#state.ops_len), State#state.ops);
                          _ ->
                              {PreviousStates, CurrentOpName} = ToDo,
                              rubis_tool:get_next_state(PreviousStates, Transition, CurrentOpName)
                    end,
                    {_, NextOpName} = NextOp,
                    TransNextOp = rubis_tool:translate_op(NextOpName),
                    OpThinkTime = op_think_time(ToDo, NextOp, ThinkTime, Transition), 
                    case deal_messages(0, FinalCdf1, SpeculaCdf1, SpeculaTxs2, ReadTxs2, MsgId) of
                        {FinalCdf2, SpeculaCdf2, SpeculaTxs3, ReadTxs3, NewMsgId, ignore, ignore} ->
                            case get_op_type(TransNextOp) of 
                                update ->  
                                    NextUpdateSeq = UpdateSeq + 1,
                                    timer:sleep(OpThinkTime),
                                    SpeculaTxs4 = SpeculaTxs3 ++ [{NextUpdateSeq, TransNextOp, os:timestamp(), ignore}],
                                    {ok, State#state{driver_state=DriverState, todo_op=NextOp, update_seq=NextUpdateSeq, last_update_cnt=NextUpdateSeq, store_cdf=StoreCdf, msg_id=NewMsgId,
                                        specula_txs=SpeculaTxs4, read_txs=ReadTxs3, specula_cdf=SpeculaCdf2, final_cdf=FinalCdf2, op_type=update}};
                                read ->
                                    NextReadSeq = ReadSeq + 1,
                                    timer:sleep(OpThinkTime),
                                    ReadTxs4 = ReadTxs3 ++ [{NextReadSeq, TransNextOp}],
                                    {ok, State#state{driver_state=DriverState, todo_op=NextOp, read_seq=NextReadSeq, msg_id=NewMsgId,
                                          specula_txs=SpeculaTxs3, read_txs=ReadTxs4, specula_cdf=SpeculaCdf2, final_cdf=FinalCdf2
                                        , store_cdf=StoreCdf, op_type=read}}
                            end;
                        {FinalCdf2, SpeculaCdf2, SpeculaTxs3, ReadTxs3, NewMsgId, RetryOpSeq, OpName1} ->
                            {ok, State#state{driver_state=DriverState, todo_op={PreviousOps, OpName1}, update_seq=RetryOpSeq, store_cdf=StoreCdf, 
                                    specula_txs=SpeculaTxs3, read_txs=ReadTxs3, specula_cdf=SpeculaCdf2, final_cdf=FinalCdf2, msg_id=NewMsgId, op_type=update}}
                    end;
                false -> %% Should redo previous operations!!! 
                    case deal_messages(0, FinalCdf1, SpeculaCdf1, SpeculaTxs2, ReadTxs2, MsgId) of
                        {FinalCdf2, SpeculaCdf2, SpeculaTxs3, ReadTxs3, NewMsgId, ignore, ignore} ->
                            NextOp = get_next_op(SpeculaTxs, UpdateSeq+1, ToDo),
                            {ok, State#state{driver_state=DriverState, todo_op=NextOp, update_seq=UpdateSeq+1, msg_id=NewMsgId, op_type=update,
                                    specula_txs=SpeculaTxs3, read_txs=ReadTxs3, specula_cdf=SpeculaCdf2, final_cdf=FinalCdf2, store_cdf=StoreCdf}};
                        {FinalCdf2, SpeculaCdf2, SpeculaTxs3, ReadTxs2, NewMsgId, RetryOpSeq, OpName1} ->
                            {ok, State#state{driver_state=DriverState, todo_op={PreviousOps, OpName1}, update_seq=RetryOpSeq, store_cdf=StoreCdf, 
                                    msg_id=NewMsgId, specula_txs=SpeculaTxs3, read_txs=ReadTxs2, specula_cdf=SpeculaCdf2, final_cdf=FinalCdf2, op_type=update}}
                    end
            end;
        %% Report abort of all cascaded txns, including the current one; also report commits of all committed.
        %% Retry txns from the aborted one.
        {cascade_abort, {AbortedTxId, AbortedReads, FinalCommitUpdates, FinalCommitReads}, DriverState} ->
            %case CurrentOpType of  update ->%lager:warning("Op ~p cascade-aborted", [UpdateSeq]); read -> ok end,
            %lager:warning("Cascade abort!!! Specula txs are ~w, abortedtx id is ~w, op aborted ~w", [SpeculaTxs, AbortedTxId, TranslatedOp]),
            State#state.shutdown_on_error andalso
                erlang:send_after(500, basho_bench,
                                  {shutdown, "Shutdown on errors requested", 1}),
            ReadTxs1 = finalize_reads(FinalCommitReads, ReadTxs, [], ok),
            ReadTxs2 = finalize_reads(AbortedReads, ReadTxs1, [], {error, abort}),
            %lager:warning("FinalReads are ~w, Specula Txs is ~w", [FinalCommitReads, SpeculaTxs]),
            {FinalCdf1, SpeculaCdf1, SpeculaTxs1} = commit_updates(FinalCdf, SpeculaCdf, FinalCommitUpdates, SpeculaTxs, []),
            %lager:warning("Aborted TxId ~w, Op Type is ~w, ReadSeq is ~w, SpeculaTxs are ~w, Previous SpeculaTxs are ~w, FinalCommitUpdates ~w, FinalCommitReads ~w", [AbortedTxId, OpTag, ReadSeq, SpeculaTxs1, SpeculaTxs, FinalCommitUpdates, FinalCommitReads]),
            {RetryOpSeq, NextOpName} = find_specula_tx(AbortedTxId, SpeculaTxs1),
            case deal_messages(0, FinalCdf1, SpeculaCdf1, SpeculaTxs1, ReadTxs2, MsgId) of
                  {FinalCdf2, SpeculaCdf2, SpeculaTxs2, ReadTxs3, NewMsgId, ignore, ignore} ->
                      {ok, State#state{driver_state=DriverState, todo_op={PreviousOps, NextOpName}, update_seq=RetryOpSeq, msg_id=NewMsgId, 
                              specula_txs=SpeculaTxs2, read_txs=ReadTxs3, specula_cdf=SpeculaCdf2, final_cdf=FinalCdf2, store_cdf=StoreCdf}};
                  {FinalCdf2, SpeculaCdf2, SpeculaTxs2, ReadTxs3, NewMsgId, RetryOpSeq1, NextOpName1} ->
                      {ok, State#state{driver_state=DriverState, todo_op={PreviousOps, NextOpName1}, update_seq=RetryOpSeq1, store_cdf=StoreCdf, 
                              msg_id=NewMsgId, specula_txs=SpeculaTxs2, read_txs=ReadTxs3, specula_cdf=SpeculaCdf2, final_cdf=FinalCdf2, op_type=update}}
            end;
        {aborted, {AbortedReads, FinalCommitUpdates, FinalCommitReads}, DriverState} ->
            %case CurrentOpType of  update ->%lager:warning("Op ~p aborted", [UpdateSeq]); read -> ok end,
            %lager:warning("Current txn ~w is aborted!!! ", [TranslatedOp]),
            State#state.shutdown_on_error andalso
                erlang:send_after(500, basho_bench,
                                  {shutdown, "Shutdown on errors requested", 1}),
            %case FinalCommitReads of [] -> ok;
            %    _ ->%lager:warning("FinalCommReads is ~w, Specula Txs is ~w", [FinalCommitReads, SpeculaTxs])
            %end,
            ReadTxs1 = finalize_reads(FinalCommitReads, ReadTxs, [], ok),
            ReadTxs2 = finalize_reads(AbortedReads, ReadTxs1, [], {error, abort}),
            {FinalCdf1, SpeculaCdf1, SpeculaTxs1} = commit_updates(FinalCdf, SpeculaCdf, FinalCommitUpdates, SpeculaTxs, []),
            case deal_messages(0, FinalCdf1, SpeculaCdf1, SpeculaTxs1, ReadTxs2, MsgId) of
                  {FinalCdf2, SpeculaCdf2, SpeculaTxs2, ReadTxs3, NewMsgId, ignore, ignore} ->
                      %% Add abort of this txn to stat, if no cascading abort was found 
                      basho_bench_stats:op_complete({TranslatedOp, TranslatedOp}, {error, cert_abort}, ElapsedUs),
                      {ok, State#state{driver_state=DriverState, todo_op=ToDo, update_seq=UpdateSeq, msg_id=NewMsgId, store_cdf=StoreCdf,
                              specula_txs=SpeculaTxs2, read_txs=ReadTxs3, specula_cdf=SpeculaCdf2, final_cdf=FinalCdf2}};
                  {FinalCdf2, SpeculaCdf2, SpeculaTxs2, ReadTxs3, NewMsgId, RetryOpSeq, OpName1} ->
                      {ok, State#state{driver_state=DriverState, todo_op={PreviousOps, OpName1}, update_seq=RetryOpSeq, store_cdf=StoreCdf, 
                              msg_id=NewMsgId, specula_txs=SpeculaTxs2, read_txs=ReadTxs3, specula_cdf=SpeculaCdf2, final_cdf=FinalCdf2, op_type=update}}
            end;
        %% Aborted due to other reasons. Just retry the current one.
        {error, Reason, DriverState} ->
            basho_bench_stats:op_complete({TranslatedOp, TranslatedOp}, {error, Reason}, ElapsedUs),
            State#state.shutdown_on_error andalso
                erlang:send_after(500, basho_bench,
                                  {shutdown, "Shutdown on errors requested", 1}),
            {ok, State#state { driver_state = DriverState, todo_op=ToDo, store_cdf=StoreCdf}};
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
                    {ok, State#state{store_cdf=StoreCdf}};
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

%% Handle final abort/final_commit message
deal_messages(ThinkTime, FinalCdf, SpeculaCdf, SpeculaTxs, ReadTxs, MsgId) ->
    deal_messages(ThinkTime, FinalCdf, SpeculaCdf, SpeculaTxs, ReadTxs, MsgId, ignore, ignore).

deal_messages(ThinkTime, FinalCdf, SpeculaCdf, SpeculaTxs, ReadTxs, MsgId, PreviousSeq, PreviousName) ->
    receive
        %% If any previous txn aborts, I also have to abort 
        {final_abort, NewMsgId, TxId, AbortedReads, FinalCommitUpdates, FinalCommitReads} -> 
            NewMsgId = MsgId+1,
            %lager:warning("New Msg Id is ~p, ~w is aborted", [NewMsgId, TxId]),
            %lager:warning("FinalComm is ~w, Specula Txs is ~w", [FinalCommitUpdates, SpeculaTxs]),
            {FinalCdf1, SpeculaCdf1, SpeculaTxs1} = 
                commit_updates(FinalCdf, SpeculaCdf, FinalCommitUpdates, SpeculaTxs, []),
            ReadTxs1 = finalize_reads(FinalCommitReads, ReadTxs, [], ok),
            ReadTxs2 = finalize_reads(AbortedReads, ReadTxs1, [], {error, abort}),
            {tx_id, _, _, _, TxSeq} = TxId,
            {TxSeq, OpName} = find_specula_tx(TxSeq, SpeculaTxs1),
            true = TxSeq < PreviousSeq, 
            deal_messages(0, FinalCdf1, SpeculaCdf1, SpeculaTxs1, ReadTxs2, NewMsgId, TxSeq, OpName)
        after ThinkTime -> 
            %% No msg is recevied. So we consider this txn is speculatively committed.
            {FinalCdf, SpeculaCdf, SpeculaTxs, ReadTxs, MsgId, PreviousSeq, PreviousName}
    end.

get_next_op([{UpdateSeq, OpName, _, _}|_T], UpdateSeq, CurrentOp) ->
    {PreviousStates, _} = CurrentOp,
    {PreviousStates, OpName};
get_next_op([_H|T], UpdateSeq, CurrentOp) ->
    get_next_op(T, UpdateSeq, CurrentOp).

op_think_time(CurrentOp, NextOp, ThinkTime, Transition) ->
    case Transition of
        undef ->
            {_, OpTag} = CurrentOp,
            {_, NewOpTag} = NextOp,
            case ThinkTime of tpcc -> 
                                tpcc_tool:get_think_time(OpTag) + tpcc_tool:get_key_time(NewOpTag);
                              _ -> ThinkTime
            end;
        _ ->
            {PreviousStates, CurrentState} = CurrentOp,
            NextToDo = rubis_tool:get_next_state(PreviousStates, Transition, CurrentState), 
            case ThinkTime of rubis -> rubis_tool:get_think_time(NextToDo, Transition);
                              _ -> ThinkTime
            end
    end.

%% Report the stat about the cascading abort after this txid. Then report its name and index
find_specula_tx(Seq, [{Seq, OpName, _StartTime, _SpecTime}|T]) ->
    report_cascade(T),
    {Seq, OpName};
find_specula_tx({tx_id, _, _, _,Seq}, [{Seq, OpName, _StartTime, _SpecTime}|T]) ->
    report_cascade(T),
    {Seq, OpName};
find_specula_tx(Seq, [{_Seq1, _OpName, _StartTime, _SpecTime}|T]) ->
    find_specula_tx(Seq, T).

report_cascade([]) ->
    ok;
report_cascade([{_Seq, OpName, _StartTime, _}|T]) ->
    %% Does elasped time matter?
    basho_bench_stats:op_complete({OpName, OpName}, {error, cascade_abort}, 0),
    report_cascade(T).


%get_next_op(undef, ThinkTime, StateOps, StateOpLen, CurrentOp, CurrentIndex, SpeculaTxs) ->
%    {_, OpTag} = CurrentOp,
%    {Info, NewOpTag} = element(random:uniform(StateOpLen), StateOps),
%    case ThinkTime of tpcc -> 
%                        {tpcc_tool:get_think_time(OpTag) + tpcc_tool:get_key_time(NewOpTag), {Info, NewOpTag}};
%                      _ -> {ThinkTime, {Info, NewOpTag}} 
%    end;
%get_next_op(Transition, ThinkTime, StateOps, StateOpLen, CurrentOp, CurrentIndex, SpeculaTxs) ->
%    {PreviousStates, CurrentState} = CurrentOp,
%    NextToDo = rubis_tool:get_next_state(PreviousStates, Transition, CurrentState), 
%    case ThinkTime of rubis -> {rubis_tool:get_think_time(NextToDo, Transition), NextToDo};
%                      _ -> {timer:sleep(ThinkTime), NextToDo}
%    end.

add_sctime_to_list([{TxnSeq, _OpName, StartTime, _}|Rest], TxnSeq, SpecCommitTime) -> 
    [{TxnSeq, _OpName, StartTime, SpecCommitTime}|Rest];
add_sctime_to_list([TxInfo|Rest], TxnSeq, SpecCommitTime) ->
    [TxInfo|add_sctime_to_list(Rest, TxnSeq, SpecCommitTime)].

commit_updates(FinalCdf, SpeculaCdf, [], SpeculaTxs, PreviousSpecula) ->
    {FinalCdf, SpeculaCdf, lists:reverse(PreviousSpecula)++SpeculaTxs};
commit_updates(FinalCdf, SpeculaCdf, [{{tx_id, _A, _B, _C, TxnSeq}, EndTime}|Rest], [{TxnSeq, OpName, StartTime, SpecTime}|SpeculaRest], PreviousSpecula)->
    UsedTime = timer:now_diff(EndTime, StartTime),
    PercvTime = timer:now_diff(SpecTime, StartTime),
    basho_bench_stats:op_complete({OpName, OpName}, ok, UsedTime),
    commit_updates([UsedTime|FinalCdf], [PercvTime|SpeculaCdf], Rest, SpeculaRest, PreviousSpecula);
commit_updates(FinalCdf, SpeculaCdf, [{tx_id, _A, _B, _C, TxnSeq}|Rest], [{TxnSeq, OpName, _StartTime, _SpecTime}|SpeculaRest], PreviousSpecula)->
    basho_bench_stats:op_complete({OpName, OpName}, ok, 0),
    commit_updates(FinalCdf, SpeculaCdf, Rest, SpeculaRest, PreviousSpecula);
%% In case of non-specula
commit_updates(FinalCdf, SpeculaCdf, [{TxnSeq, EndTime}|Rest], [{TxnSeq, OpName, StartTime, ignore}|SpeculaRest], PreviousSpecula)->
    UsedTime = timer:now_diff(EndTime, StartTime),
    basho_bench_stats:op_complete({OpName, OpName}, ok, UsedTime),
    commit_updates([UsedTime|FinalCdf], SpeculaCdf, Rest, SpeculaRest, PreviousSpecula); 
commit_updates(FinalCdf, SpeculaCdf, [{TxnSeq, EndTime}|Rest], [{TxnSeq, OpName, StartTime, SpecTime}|SpeculaRest], PreviousSpecula)->
    UsedTime = timer:now_diff(EndTime, StartTime),
    PercvTime = timer:now_diff(SpecTime, StartTime),
    basho_bench_stats:op_complete({OpName, OpName}, ok, UsedTime),
    commit_updates([UsedTime|FinalCdf], [PercvTime|SpeculaCdf], Rest, SpeculaRest, PreviousSpecula); 
commit_updates(FinalCdf, SpeculaCdf, List, [Entry|SpeculaRest], PreviousSpecula) ->
    commit_updates(FinalCdf, SpeculaCdf, List, SpeculaRest, [Entry|PreviousSpecula]).

finalize_reads([], ReadTxs, Previous, _Result) ->
    lists:reverse(Previous)++ReadTxs;
finalize_reads([{tx_id, _,_,_,TxnSeq}|T], [{TxnSeq, OpName}|Rest],Previous, Result) ->
    basho_bench_stats:op_complete({OpName, OpName}, Result, 0),
    finalize_reads(T, Rest, Previous, Result);
finalize_reads([TxnSeq|T], [{TxnSeq, OpName}|Rest], Previous, Result) ->
    basho_bench_stats:op_complete({OpName, OpName}, Result, 0),
    finalize_reads(T, Rest, Previous, Result);
finalize_reads(List, [Entry|SpeculaRest], PreviousSpecula, Result) ->
    finalize_reads(List, SpeculaRest, [Entry|PreviousSpecula], Result).

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
        %io:format("~p :my operations are ~p", [self(), lists:reverse(State#state.op_list)]), 
        %write_to_file(State),
	    (catch (State#state.driver):terminate(normal,
						  State#state.driver_state)),
	    true
    after 0 ->
            false
    end.

write_to_file(State) ->
    {Cnt, _StartTime, _Period} = State#state.store_cdf,
    MySelf = self(),
    lists:foreach(fun(Count) ->
                {ok, PercvFile} = file:open("percv_latency"++integer_to_list(Count), [raw, binary, append]),
                {ok, FinalFile} = file:open("final_latency"++integer_to_list(Count), [raw, binary, append]),
                %lager:info("Key is ~p, latency is ~p", [Key, LatencyList]),
                [{{MySelf, Count}, PercvList}] = ets:lookup(percv_cdf, {MySelf, Count}),
                [{{MySelf, Count}, FinalList}] = ets:lookup(final_cdf, {MySelf, Count}),
                file:write(FinalFile,  io_lib:format("~w\n", [self()])),
                file:write(PercvFile,  io_lib:format("~w\n", [self()])),
                lists:foreach(fun(Latency) ->
                          file:write(FinalFile,  io_lib:format("~w\n", [Latency]))
                end, FinalList),
                lists:foreach(fun(Latency) ->
                          file:write(PercvFile,  io_lib:format("~w\n", [Latency]))
                end, PercvList),
                file:close(PercvFile),
                file:close(FinalFile)
                end, lists:seq(1, Cnt-1)).

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
            %lager:info("CDF is ~w", [State#state.cdf]),
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

get_op_type(register_user) ->
    update;
get_op_type(register_item) ->
    update;
get_op_type(store_bid) ->
    update;
get_op_type(store_comment) ->
    update;
get_op_type(store_buy_now) ->
    update;
get_op_type(txn) ->
    update;
get_op_type(new_order) ->
    update;
get_op_type(payment) ->
    update;
get_op_type(order_status) ->
    read;
get_op_type({_, register_user}) ->
    update;
get_op_type({_, register_item}) ->
    update;
get_op_type({_, store_bid}) ->
    update;
get_op_type({_, store_comment}) ->
    update;
get_op_type({_, store_buy_now}) ->
    update;
get_op_type({_, txn}) ->
    update;
get_op_type({_, new_order}) ->
    update;
get_op_type({_, payment}) ->
    update;
get_op_type({_, order_status}) ->
    read;
get_op_type(_Name) ->
    read.
    
