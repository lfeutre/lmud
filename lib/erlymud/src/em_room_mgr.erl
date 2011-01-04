-module(em_room_mgr).

-behaviour(gen_server).

-export([start_link/0, get_room/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE_ID, ?MODULE).

-record(state, {}).

%% API

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

% Concurrent lookup if room is loaded; call to em_room_mgr otherwise
get_room(Name) ->
  case ets:lookup(?TABLE_ID, Name) of
    [{Name, Room}] ->
      {ok, Room};
    [] ->
      gen_server:call(?SERVER, {get_room, Name})
  end.

%% gen_server callbacks

init([]) ->
  process_flag(trap_exit, true),
  ets:new(?TABLE_ID, [protected, named_table]),
  {ok, #state{}}.

handle_call({get_room, Name}, _From, State) ->
  case try_load_room(Name) of
    {ok, Room} ->
      % For now a link; we're lost if em_room_mgr dies and the Rooms
      % dict disappears, so all rooms should be killed
      link(Room),
      {reply, {ok, Room}, State};
    {error, not_found} ->
      {reply, {error, not_found}, State}
  end.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'EXIT', From, Reason}, State) ->
  case ets:match(?TABLE_ID, {'$1', From}) of
    [[Name]] ->
      ets:delete(?TABLE_ID, Name),
      {noreply, State};
    [] ->
      {stop, Reason, State}
  end.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions

try_load_room(Name) ->
  RoomFile = filename:join([code:priv_dir(erlymud), "rooms", 
                            Name ++ ".dat"]),
  load_room(RoomFile).

load_room(Filename) ->
  io:format("loading: ~s~n", [Filename]),
  case file:consult(Filename) of
    {ok, Data} ->
      {Name, Room} = make_room(Filename, Data),
      ets:insert(?TABLE_ID, {Name, Room}),
      {ok, Room};
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
