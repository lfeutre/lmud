%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Embodiment of player characters in the game.
%%% This gen_server represents player characters, holding the necessary state
%%% and acting upon the world as directed by the user's commands.
%%% @end
%%% =========================================================================
-module(em_living).

-behaviour(gen_server).

%% API
-export([start_link/2, start/2, stop/1, 
         get_name/1, 
         get_room/1, set_room/2,
         add_object/2, move_object/3, get_objects/1, remove_object/2,
         set_long/2, long/1,
         cmd/2, print/2, print/3,
         load/1, save/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(state, {name="noname" :: living_name(), room::em_room:room_pid(), 
                client::client(), long="" :: string(), 
                objects=[] :: [em_object:object()]}).

%% ==========================================================================
%% Type Specifications
%% ==========================================================================
-include("types.hrl").
-type living_name() :: string().
-type living_pid() :: pid().
-type client() :: {em_user:user_pid(), em_conn:conn_pid()}.

-export_type([living_name/0, living_pid/0, client/0]).


%% ==========================================================================
%% API functions
%% ==========================================================================

-spec start_link(living_name(), client()) -> any().
start_link(Name, Client) ->
  gen_server:start_link(?MODULE, [Name, Client], []).

-spec start(living_name(), client()) -> any().
start(Name, Client) ->
  gen_server:start(?MODULE, [Name, Client], []).

-spec get_name(living_pid()) -> living_name().
get_name(Pid) ->
  gen_server:call(Pid, get_name).

-spec get_room(living_pid()) -> em_room:room_pid()|undefined.
get_room(Pid) ->
  gen_server:call(Pid, get_room).

-spec add_object(living_pid(), em_object:object()) -> ok.
add_object(Pid, Ob) ->
  gen_server:call(Pid, {add_object, Ob}).

-spec move_object(living_pid(), em_object:object(), 
                  {to_room, em_room:room_pid()}) -> ok.
move_object(Pid, Ob, Dest) ->
  gen_server:call(Pid, {move_object, Ob, Dest}).

-spec get_objects(living_pid()) -> [em_object:object()].
get_objects(Pid) ->
  gen_server:call(Pid, get_objects).

-spec remove_object(living_pid(), em_object:object()) -> ok.
remove_object(Pid, Ob) ->
  gen_server:call(Pid, {remove_object, Ob}).

-spec set_room(living_pid(), em_room:room_name() | em_room:room_pid()) -> ok.
set_room(Pid, Room) when is_pid(Room) ->
  gen_server:call(Pid, {set_room_pid, Room});
set_room(Pid, Room) when is_list(Room) ->
  gen_server:call(Pid, {set_room_str, Room}).

-spec set_long(living_pid(), string()) -> ok.
set_long(Pid, Long) ->
  gen_server:call(Pid, {set_long, Long}).

-spec long(living_pid()) -> string().
long(Pid) ->
  gen_server:call(Pid, long).

-spec cmd(living_pid(), string()) -> ok.
cmd(Pid, Line) ->
  gen_server:call(Pid, {cmd, Line}).

-spec print(living_pid(), iolist()) -> ok.
print(Pid, Format) ->
  gen_server:call(Pid, {print, Format}).

-spec print(living_pid(), iolist(), list()) -> ok.
print(Pid, Format, Args) ->
  gen_server:call(Pid, {print, Format, Args}).

-spec stop(living_pid()) -> ok.
stop(Pid) ->
  gen_server:cast(Pid, stop).

-spec load(living_pid()) -> ok|{error, not_found}.
load(Pid) ->
  gen_server:call(Pid, load).

-spec save(living_pid()) -> ok|{error, any()}.
save(Pid) ->
  gen_server:call(Pid, save).


%% ==========================================================================
%% gen_server callbacks
%% ==========================================================================

init([Name, Client]) ->
  {ok, #state{name=Name, client=Client}}.

handle_call(get_name, _From, #state{name=Name}=State) ->
  {reply, Name, State};
handle_call(get_room, _From, #state{room=Room}=State) ->
  {reply, Room, State};
handle_call({add_object, Ob}, _From, #state{objects=Obs}=State) ->
  {reply, ok, State#state{objects=[Ob|Obs]}};
handle_call({move_object, Ob, Dest}, _From, State) ->
  {Result, NewState} = do_move_object(Ob, Dest, State),
  {reply, Result, NewState};
handle_call(get_objects, _From, #state{objects=Obs}=State) ->
  {reply, Obs, State};
handle_call({remove_object, Ob}, _From, State) ->
  {Result, NewState} = do_remove_object(Ob, State),
  {reply, Result, NewState};
handle_call({set_room_pid, Room}, _From, State) ->
  {reply, ok, State#state{room=Room}};
handle_call({set_room_str, RoomStr}, _From, State) ->
  {ok, Room} = em_room_mgr:get_room(RoomStr),
  {reply, ok, State#state{room=Room}};
handle_call({set_long, Long}, _From, State) ->
  {reply, ok, State#state{long=Long}};
handle_call(long, _From, #state{long=Long}=State) ->
  {reply, Long, State};
handle_call({print, Format}, _From, #state{client={_,Out}}=State) ->
  em_conn:print(Out, Format),
  {reply, ok, State};
handle_call({print, Format, Args}, _From, #state{client={_,Out}}=State) ->
  em_conn:print(Out, Format, Args),
  {reply, ok, State};
handle_call(load, _From, State) ->
  case do_load(State) of
    {ok, NewState} ->
      {reply, ok, NewState};
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end;
handle_call(save, _From, State) ->
  case do_save(State) of
    {ok, NewState} ->
      {reply, ok, NewState};
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end.

handle_cast(stop, #state{client={_,Out}}=State) ->
  em_conn:print(Out, "living(): stopping.~n"),
  {stop, normal, ok, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% ==========================================================================
%% Internal functions
%% ==========================================================================

-spec do_move_object(em_object:object(), {to_room, em_room:room_pid()}, 
                     #state{}) -> {ok, #state{}}.
do_move_object(Ob, {to_room, Room}, #state{objects=Obs}=State) ->
  case lists:member(Ob, Obs) of
    false -> throw(not_found);
    true -> 
      NewObs = lists:delete(Ob, Obs),
      ok = em_room:add_object(Room, Ob),
      {ok, State#state{objects = NewObs}}
  end.

-spec do_remove_object(em_object:object(), #state{}) -> 
        {ok, #state{}} | {{error, not_found}, #state{}}.
do_remove_object(Ob, #state{objects=Obs}=State) ->
  do_remove_object(Ob, State, Obs, []).

-spec do_remove_object(em_object:object(), #state{}, [em_object:object()],
                       [em_object:object()]) -> 
        {ok, #state{}} | {{error, not_found}, #state{}}.
do_remove_object(_Ob, State, [], _Searched) ->
  {{error, not_found}, State};
do_remove_object(Ob, State, [Ob|Obs], Searched) ->
  {ok, State#state{objects=Obs ++ Searched}};
do_remove_object(Ob, State, [NoMatch|Obs], Searched) ->
  do_remove_object(Ob, State, Obs, [NoMatch|Searched]).

%% Load

-spec do_load(#state{}) -> {ok, #state{}} | {error, not_found}.
do_load(#state{name=Name}=State) ->
  File = make_filename(Name),
  load_living(File, State).

-spec make_filename(string()) -> file_path().
make_filename(Name) ->
  filename:join([em_game:data_dir(), "livings", Name ++ ".dat"]).

-spec load_living(file_path(), #state{}) -> 
        {ok, #state{}} | {error, not_found}.
load_living(Filename, State) ->
  io:format("loading living: ~s~n", [Filename]),
  case file:consult(Filename) of
    {ok, Data} ->
      NewState = update_living(Data, State),
      {ok, NewState};
    {error, _Reason} ->
      {error, not_found}
  end.

-spec update_living([{any(), any()}], #state{}) -> #state{}.
update_living([], State) ->
  State;
update_living([{long, Long}|Data], State) ->
  update_living(Data, State#state{long=Long});
update_living([{room, Room}|Data], State) ->
  {ok, RoomPid} = em_room_mgr:get_room(Room),
  update_living(Data, State#state{room=RoomPid});
update_living([{objects, ObList}|Data], State) ->
  update_living(Data, State#state{objects=em_object:load_obs(ObList)});
update_living([_Other|Data], State) ->
  update_living(Data, State).

-spec do_save(#state{}) -> {ok, #state{}} | {error, any()}.
do_save(#state{name=Name}=State) ->
  Data = save_living(State),
  File = make_filename(Name),
  case file:write_file(File, Data) of
    ok ->
      {ok, State};
    {error, Reason} ->
      {error, file:format_error(Reason)}
  end.

-spec save_living(#state{}) -> string().
save_living(State) ->
  lists:flatten([
    "{version, 1}.\n",
    "{long, \"", State#state.long, "\"}.\n",
    "{room, \"", em_room:get_name(State#state.room), "\"}.\n",
    "{objects, ", save_objects(State), "}.\n"
  ]).

-spec save_objects(#state{}) -> iolist().
save_objects(State) ->
  Obs = State#state.objects,
  ObTemplates = [em_object:get_template(Ob)||Ob <- Obs],
  save_stringlist(ObTemplates).

-spec save_stringlist([string()]) -> iolist().
save_stringlist(List) ->
  ["[", string:join([["\"",Str,"\""]||Str <- List], ", "), "]"].
  
