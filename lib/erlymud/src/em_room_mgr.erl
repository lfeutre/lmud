-module(em_room_mgr).

-behaviour(gen_server).

-export([start_link/0, get_room/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {rooms}).

%% API

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_room(Name) ->
  gen_server:call(?SERVER, {get_room, Name}).

%% gen_server callbacks

init([]) ->
  % Rooms = build_world(),
  % Rooms = load_world(),
  Rooms = dict:new(),
  {ok, #state{rooms=Rooms}}.

handle_call({get_room, Name}, _From, #state{rooms=Rooms}=State) ->
  case dict:is_key(Name, Rooms) of
    true ->
      {reply, dict:fetch(Name, Rooms), State};
    false ->
      case try_load_room(Name, Rooms) of
        {ok, Room, NewRooms} ->
          {reply, Room, State#state{rooms=NewRooms}};
        {error, not_found} ->
          {reply, {error, not_found}, State}
      end
  end.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions

-ifdef(false).
build_world() ->
  {ok, R1} = em_room_pool_sup:start_child(
      "A small room",
      "This is a small, rather non-descript room. To the east is a corridor."),
  {ok, R2} = em_room_pool_sup:start_child(
      "A long, dark corridor",
      "This dark, damp corridor continues to the north and south."),
  em_room:add_exit(R1, "east", "room2"),
  em_room:add_exit(R2, "west", "room1"),
  {ok, R3} = em_room_pool_sup:start_child(
      "A long, dark corridor",
      "This dark, damp corridor continues to the north and south."),
  em_room:add_exit(R2, "north", "room3"),
  em_room:add_exit(R3, "south", "room2"),
  add_rooms([{"room1", R1}, {"room2", R2}, {"room3", R3}], dict:new()).

add_rooms([], Dict) ->
  Dict;
add_rooms([{Name, Room}|Rooms], Dict) ->
  NewDict = dict:store(Name, Room, Dict),
  add_rooms(Rooms, NewDict).
-endif.

%load_world() ->
%  RoomDir = filename:join([code:priv_dir(erlymud), "rooms"]),
%  filelib:fold_files(RoomDir, "\.dat$", false, fun load_room/2, dict:new()).

try_load_room(Name, Rooms) ->
  RoomFile = filename:join([code:priv_dir(erlymud), "rooms", 
                            Name ++ ".dat"]),
  load_room(RoomFile, Rooms).

load_room(Filename, Rooms) ->
  io:format("loading: ~s~n", [Filename]),
  case file:consult(Filename) of
    {ok, Data} ->
      {Name, Room} = make_room(Filename, Data),
      {ok, Room, dict:store(Name, Room, Rooms)};
    {error, _Reason} ->
      {error, not_found}
  end.
 
make_room(Filename, Data) ->
  Name = extract_name(string:tokens(Filename, "/")),
  {title, Title} = lists:keyfind(title, 1, Data),
  {desc, Desc} = lists:keyfind(desc, 1, Data),
  {exits, Exits} = lists:keyfind(exits, 1, Data),
  {ok, Room} = em_room_pool_sup:start_child(Title, Desc),
  add_exits(Room, Exits),
  {Name, Room}.

add_exits(Room, []) ->
  Room;
add_exits(Room, [{Dir, Dest}|Exits]) ->
  em_room:add_exit(Room, Dir, Dest),
  add_exits(Room, Exits).

extract_name([Basename]) ->
  re:replace(Basename, "\.dat$", "", [{return, list}]);
extract_name([_|T]) ->
  extract_name(T).
