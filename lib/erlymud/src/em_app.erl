-module(em_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case em_sup:start_link() of
      {ok, Pid} ->
        % Set up the game world
        build_world(),
        % Allow connections
        em_conn_sup:start_child(),
        {ok, Pid};
      Other ->
        {error, Other}
    end.

stop(_State) ->
    ok.

%% Internal functions

build_world() ->
  {ok, R1} = em_room_sup:start_child(
      "A small room",
      "This is a small, rather non-descript room. To the east is a corridor."),
  {ok, R2} = em_room_sup:start_child(
      "A long, dark corridor",
      "This dark, damp corridor continues to the north and south."),
  em_room:add_exit(R1, "east", R2),
  em_room:add_exit(R2, "west", R1),
  {ok, R3} = em_room_sup:start_child(
      "A long, dark corridor",
      "This dark, damp corridor continues to the north and south."),
  em_room:add_exit(R2, "north", R3),
  em_room:add_exit(R3, "south", R2),
  em_game:add_rooms([R1, R2, R3]).
