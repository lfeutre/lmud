-module(em_spell_ward).
-behaviour(em_spell).
-behaviour(em_event_listener).

%% API
-export([start/2]).

%% em_spell exports
-export([init/1, handle_cast/2, handle_info/2, terminate/2]).

%% em_event_listener exports
-export([handle_event/1]).

-record(state, {caster, room}).

%% API

start(Caster, Room) ->
  em_spell_sup:start_child(?MODULE, [Caster, Room]).

%% em_spell exports

init([Caster, Room]) ->
  {ok, #state{caster=Caster, room=Room}, 0}.

handle_cast({handle_event, Event}, State) ->
  do_handle_event(Event, State).

handle_info(timeout, State) ->
  em_room:add_event_listener(State#state.room, {em_spell_ward, [self()]}),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

%% em_event_listener exports

handle_event([Pid, Event]) ->
  gen_server:cast(Pid, {handle_event, Event}).

%% internal

do_handle_event({enter_room, _Liv}, State) ->
  Caster = State#state.caster,
  em_living:print(Caster, "A tingling sensation tells you that a ward has "
                          "been tripped.\n"),
  {stop, normal, State};
do_handle_event(_Event, State) ->
  {noreply, State}.
