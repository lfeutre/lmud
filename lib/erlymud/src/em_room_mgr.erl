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
  process_flag(trap_exit, true),
  Rooms = dict:new(),
  {ok, #state{rooms=Rooms}}.

handle_call({get_room, Name}, _From, #state{rooms=Rooms}=State) ->
  case dict:is_key(Name, Rooms) of
    true ->
      {reply, dict:fetch(Name, Rooms), State};
    false ->
      case try_load_room(Name, Rooms) of
        {ok, Room, NewRooms} ->
          % For now a link; we're lost if em_room_mgr dies and the Rooms
          % dict disappears, so all rooms should be killed
          link(Room),
          {reply, Room, State#state{rooms=NewRooms}};
        {error, not_found} ->
          {reply, {error, not_found}, State}
      end
  end.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'EXIT', From, Reason}, #state{rooms=Rooms}=State) ->
  List = dict:to_list(Rooms),
  case lists:keyfind(From, 2, List) of
    {Name, From} ->
      NewRooms = dict:erase(Name, Rooms),
      {noreply, State#state{rooms=NewRooms}};
    false ->
      {stop, Reason, State}
  end.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions

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
