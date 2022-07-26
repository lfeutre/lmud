%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% =========================================================================
-module(mm_spell).

-include("apps/lmud/include/state.hrl").
-include("apps/lmud/include/types.hrl").

-behaviour(gen_server).

%% API exports
-export([start_link/2]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
  [{init,1},
   {handle_cast, 2},
   {handle_info, 2},
   {terminate, 2}];
behaviour_info(_Other) ->
    undefined.

%% ==========================================================================
%% API Functions
%% ==========================================================================

-spec start_link(atom(), list()) ->
  {ok, pid()} | {error, term()} | ignore.
start_link(Mod, Args) ->
  gen_server:start_link(?MODULE, [Mod, Args], []).


%% ==========================================================================
%% gen_server callbacks
%% ==========================================================================

-spec init(list()) -> {ok, #state_speller{}} | {ok, #state_speller{}, integer()}.
init([Mod, Args]) ->
  case Mod:init(Args) of
    {ok, SpellState} ->
      {ok, #state_speller{spell_mod = Mod, spell_state = SpellState}};
    {ok, SpellState, Timeout} ->
      {ok, #state_speller{spell_mod = Mod, spell_state = SpellState}, Timeout}
  end.

%% @doc We don't listen to any calls, just empty the queue.
-spec handle_call(any(), pid(), #state_speller{}) -> {reply, ok, #state_speller{}}.
handle_call(_Req, _From, State) ->
  {reply, ok, State}.

%% @doc We don't listen to any casts, just empty the queue.
-spec handle_cast(any(), #state_speller{}) -> {noreply, #state_speller{}}.
handle_cast(Msg, State) ->
  Mod = State#state_speller.spell_mod,
  SpellState = State#state_speller.spell_state,
  case Mod:handle_cast(Msg, SpellState) of
    {noreply, NewSpellState} ->
      {noreply, State#state_speller{spell_state=NewSpellState}};
    {stop, Reason, _NewSpellState} ->
      {stop, Reason, State}
  end.

%% @doc Pass other messages on to the spell module.
handle_info(Info, State) ->
  Mod = State#state_speller.spell_mod,
  SpellState = State#state_speller.spell_state,
  case Mod:handle_info(Info, SpellState) of
    {noreply, NewSpellState} ->
      {noreply, State#state_speller{spell_state=NewSpellState}};
    {stop, Reason, _NewSpellState} ->
      {stop, Reason, State}
  end.

-spec terminate(any(), #state_speller{}) -> ok.
terminate(Reason, State) ->
  Mod = State#state_speller.spell_mod,
  SpellState = State#state_speller.spell_state,
  Mod:terminate(Reason, SpellState),
  ok.

-spec code_change(string(), #state_speller{}, any()) -> {ok, #state_speller{}}.
code_change(_Vsn, State, _Extra) ->
  {ok, State}.

