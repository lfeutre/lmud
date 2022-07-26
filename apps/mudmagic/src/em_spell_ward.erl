-module(em_spell_ward).
-behaviour(em_spell).
-behaviour('lmud-event-listener').

%% API
-export([start/2]).

%% em_spell exports
-export([init/1, handle_cast/2, handle_info/2, terminate/2]).

%% lmud-event-listener exports
-export([handle_event/1]).

-include("apps/lmud/include/state.hrl").

%% API

start(Caster, Room) ->
  'lmud-spell-sup':start_child(?MODULE, [Caster, Room]).

%% em_spell exports

init([Caster, Room]) ->
  {ok, #state_speller{caster=Caster, room=Room}, 0}.

handle_cast({handle_event, Event}, State) ->
  do_handle_event(Event, State).

handle_info(timeout, State) ->
  lmud_room:add_event_listener(State#state_speller.room, {em_spell_ward, [self()]}),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

%% lmud-event-listener exports

handle_event([Pid, Event]) ->
  gen_server:cast(Pid, {handle_event, Event}).

%% internal

do_handle_event({enter_room, _Liv}, State) ->
  Caster = State#state_speller.caster,
  lmud_character:print(Caster, "A tingling sensation tells you that a ward has "
                          "been tripped.\n"),
  {stop, normal, State};
do_handle_event(_Event, State) ->
  {noreply, State}.
