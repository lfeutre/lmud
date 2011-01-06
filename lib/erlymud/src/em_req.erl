-module(em_req).

-behaviour(gen_server).

-export([start_link/1, run/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {mfa}).


%% API

start_link(MFA) ->
  gen_server:start_link(?MODULE, [MFA], []).

run(Pid) ->
  gen_server:call(Pid, run).


%% gen_server callbacks

init([MFA]) ->
  process_flag(trap_exit, true),
  {ok, #state{mfa=MFA}}.

handle_call(run, _From, #state{mfa={M, F, A}}=State) ->
  Result = apply(M, F, A),
  {reply, Result, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'EXIT', _From, normal}, State) ->
  {stop, normal, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

