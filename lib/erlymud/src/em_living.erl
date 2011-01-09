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

-record(state, {name, room, client, long="", objects=[]}).


%% API functions

start_link(Name, Client) ->
  gen_server:start_link(?MODULE, [Name, Client], []).

start(Name, Client) ->
  gen_server:start(?MODULE, [Name, Client], []).

get_name(Pid) ->
  gen_server:call(Pid, get_name).

get_room(Pid) ->
  gen_server:call(Pid, get_room).

add_object(Pid, Ob) ->
  gen_server:call(Pid, {add_object, Ob}).

%% Dest = {to_room, <RoomPid>}
move_object(Pid, Ob, Dest) ->
  gen_server:call(Pid, {move_object, Ob, Dest}).

get_objects(Pid) ->
  gen_server:call(Pid, get_objects).

remove_object(Pid, Ob) ->
  gen_server:call(Pid, {remove_object, Ob}).

set_room(Pid, Room) when is_pid(Room) ->
  gen_server:call(Pid, {set_room_pid, Room});
set_room(Pid, Room) when is_list(Room) ->
  gen_server:call(Pid, {set_room_str, Room}).

set_long(Pid, Long) ->
  gen_server:call(Pid, {set_long, Long}).

long(Pid) ->
  gen_server:call(Pid, long).

cmd(Pid, Line) ->
  gen_server:call(Pid, {cmd, Line}).

print(Pid, Format) ->
  gen_server:call(Pid, {print, Format}).

print(Pid, Format, Args) ->
  gen_server:call(Pid, {print, Format, Args}).

stop(Pid) ->
  gen_server:cast(Pid, stop).

load(Pid) ->
  gen_server:call(Pid, load).

save(Pid) ->
  gen_server:call(Pid, save).


% gen_server callbacks

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


%% Internal functions

do_move_object(Ob, {to_room, Room}, #state{objects=Obs}=State) ->
  case lists:member(Ob, Obs) of
    false -> throw(not_found);
    true -> 
      NewObs = lists:delete(Ob, Obs),
      ok = em_room:add_object(Room, Ob),
      {ok, State#state{objects = NewObs}}
  end.

do_remove_object(Ob, #state{objects=Obs}=State) ->
  do_remove_object(Ob, State, Obs, []).

do_remove_object(_Ob, State, [], _Searched) ->
  {{error, not_found}, State};
do_remove_object(Ob, State, [Ob|Obs], Searched) ->
  {ok, State#state{objects=Obs ++ Searched}};
do_remove_object(Ob, State, [NoMatch|Obs], Searched) ->
  do_remove_object(Ob, State, Obs, [NoMatch|Searched]).

%% Load

do_load(#state{name=Name}=State) ->
  File = filename:join([code:priv_dir(erlymud), "livings",
                        Name ++ ".dat"]),
  load_living(File, State).

load_living(Filename, State) ->
  io:format("loading living: ~s~n", [Filename]),
  case file:consult(Filename) of
    {ok, Data} ->
      NewState = update_living(Data, State),
      {ok, NewState};
    {error, _Reason} ->
      {error, not_found}
  end.

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

do_save(#state{name=Name}=State) ->
  Data = save_living(State),
  File = filename:join([code:priv_dir(erlymud), "livings",
                        Name ++ ".dat"]),
  case file:write_file(File, Data) of
    ok ->
      {ok, State};
    {error, Reason} ->
      {error, file:format_error(Reason)}
  end.

save_living(State) ->
  lists:flatten([
    "{version, 1}.\n",
    "{long, \"", State#state.long, "\"}.\n",
    "{room, \"", em_room:get_name(State#state.room), "\"}.\n",
    "{objects, ", save_objects(State), "}.\n"
  ]).

save_objects(State) ->
  Obs = State#state.objects,
  ObTemplates = [em_object:get_template(Ob)||Ob <- Obs],
  save_stringlist(ObTemplates).

save_stringlist(List) ->
  ["[", string:join([["\"",Str,"\""]||Str <- List], ", "), "]"].
  
