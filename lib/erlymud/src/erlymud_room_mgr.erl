-module(erlymud_room_mgr).

-behaviour(gen_server).

-export([start_link/0, lookup/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {rooms}).

-define(SERVER, ?MODULE).


%% API functions

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

lookup(Name) ->
  gen_server:call(?SERVER, {lookup, Name}).


%% Gen_server callbacks

init([]) ->
  Room = erlymud_room:create("A small room", "This is a fairly small, non-descript room."),
  R1 = dict:new(),
  R2 = dict:store("room1", Room, R1),
  {ok, #state{rooms = R2}}.

handle_call({lookup, Name}, _From, State) ->
  Reply = case dict:is_key(Name, State#state.rooms) of
            true ->
              Room = dict:fetch(Name, State#state.rooms),
              {ok, Room};
            false ->
              {error, not_found}
          end,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

