%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Process a single request.
%%% This gen_server is initiated with an MFA to call, and will do so when
%%% the run() function is evaluated. Intended to be called only from the
%%% request supervisor, 'lmud-req-sup':request(), which will spawn us as a
%%% process, evaluate run(), and then kill the process.
%%% @end
%%% =========================================================================
-module(msh_req).

-behaviour(gen_server).

%% API
-export([start_link/1, run/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {mfa :: mfargs()}).

%% Type Specifications
-include("apps/lmud/include/types.hrl").


%% ==========================================================================
%% API Functions
%% ==========================================================================

-spec start_link(mfargs()) -> any().
start_link(MFA) ->
  gen_server:start_link(?MODULE, [MFA], []).

-spec run(pid()) -> any().
run(Pid) ->
  gen_server:call(Pid, run).


%% ==========================================================================
%% gen_server callbacks
%% ==========================================================================

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

