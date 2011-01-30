%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% =========================================================================
-module(em_spell).
-include("types.hrl").

-behaviour(gen_server).

%% API exports
-export([start_link/2]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {spell_mod :: atom(), spell_state :: any()}).

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

-spec init(list()) -> {ok, #state{}} | {ok, #state{}, integer()}.
init([Mod, Args]) ->
  case Mod:init(Args) of
    {ok, SpellState} ->
      {ok, #state{spell_mod = Mod, spell_state = SpellState}};
    {ok, SpellState, Timeout} ->
      {ok, #state{spell_mod = Mod, spell_state = SpellState}, Timeout}
  end.

%% @doc We don't listen to any calls, just empty the queue.
-spec handle_call(any(), pid(), #state{}) -> {reply, ok, #state{}}.
handle_call(_Req, _From, State) ->
  {reply, ok, State}.

%% @doc We don't listen to any casts, just empty the queue.
-spec handle_cast(any(), #state{}) -> {noreply, #state{}}.
handle_cast(Msg, State) ->
  Mod = State#state.spell_mod,
  SpellState = State#state.spell_state,
  case Mod:handle_cast(Msg, SpellState) of
    {noreply, NewSpellState} ->
      {noreply, State#state{spell_state=NewSpellState}};
    {stop, Reason, _NewSpellState} ->
      {stop, Reason, State}
  end.

%% @doc Pass other messages on to the spell module.
handle_info(Info, State) ->
  Mod = State#state.spell_mod,
  SpellState = State#state.spell_state,
  case Mod:handle_info(Info, SpellState) of
    {noreply, NewSpellState} ->
      {noreply, State#state{spell_state=NewSpellState}};
    {stop, Reason, _NewSpellState} ->
      {stop, Reason, State}
  end.

-spec terminate(any(), #state{}) -> ok.
terminate(Reason, State) ->
  Mod = State#state.spell_mod,
  SpellState = State#state.spell_state,
  Mod:terminate(Reason, SpellState),
  ok.

-spec code_change(string(), #state{}, any()) -> {ok, #state{}}.
code_change(_Vsn, State, _Extra) ->
  {ok, State}.

