%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Embodiment of playing characters in the game.
%%% This gen_server represents playing characters, holding the necessary state
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
         set_desc/2, desc/1,
         cmd/2, print/2, print/3,
         load/1, save/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("logjam/include/logjam.hrl").
-include_lib("apps/lmud/include/state.hrl").

%% ==========================================================================
%% Type Specifications
%% ==========================================================================
-include("apps/lmud/include/types.hrl").
-type living_name() :: string().
-type living_pid() :: pid().

-export_type([living_name/0, living_pid/0]).


%% ==========================================================================
%% API functions
%% ==========================================================================

start_link(Name, Client) ->
  gen_server:start_link(?MODULE, [Name, Client], []).

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

-spec set_desc(living_pid(), string()) -> ok.
set_desc(Pid, Desc) ->
  gen_server:call(Pid, {set_desc, Desc}).

-spec desc(living_pid()) -> string().
desc(Pid) ->
  gen_server:call(Pid, desc).

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
  {ok, #state_living{name=Name, client=Client}}.

handle_call(get_name, _From, #state_living{name=Name}=State) ->
  {reply, Name, State};
handle_call(get_room, _From, #state_living{room=Room}=State) ->
  {reply, Room, State};
handle_call({add_object, Ob}, _From, #state_living{objects=Obs}=State) ->
  {reply, ok, State#state_living{objects=[Ob|Obs]}};
handle_call({move_object, Ob, Dest}, _From, State) ->
  {Result, NewState} = do_move_object(Ob, Dest, State),
  {reply, Result, NewState};
handle_call(get_objects, _From, #state_living{objects=Obs}=State) ->
  {reply, Obs, State};
handle_call({remove_object, Ob}, _From, State) ->
  {Result, NewState} = do_remove_object(Ob, State),
  {reply, Result, NewState};
handle_call({set_room_pid, Room}, _From, State) ->
  {reply, ok, State#state_living{room=Room}};
handle_call({set_room_str, RoomStr}, _From, State) ->
  {ok, Room} = em_room_mgr:get_room(RoomStr),
  {reply, ok, State#state_living{room=Room}};
handle_call({set_desc, Desc}, _From, State) ->
  {reply, ok, State#state_living{desc=Desc}};
handle_call(desc, _From, #state_living{desc=Desc}=State) ->
  {reply, Desc, State};
handle_call({print, Format}, _From, #state_living{client={_,Out}}=State) ->
  em_conn:print(Out, Format),
  {reply, ok, State};
handle_call({print, Format, Args}, _From, #state_living{client={_,Out}}=State) ->
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
  ?'log-debug'("preparing to save ..."),
  case do_save(State) of
    {ok, NewState} ->
      {reply, ok, NewState};
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end.

handle_cast(stop, #state_living{client={_,Out}}=State) ->
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
                     #state_living{}) -> {ok, #state_living{}}.
do_move_object(Ob, {to_room, Room}, #state_living{objects=Obs}=State) ->
  case lists:member(Ob, Obs) of
    false -> throw(not_found);
    true ->
      NewObs = lists:delete(Ob, Obs),
      ok = em_room:add_object(Room, Ob),
      {ok, State#state_living{objects = NewObs}}
  end.

-spec do_remove_object(em_object:object(), #state_living{}) ->
        {ok, #state_living{}} | {{error, not_found}, #state_living{}}.
do_remove_object(Ob, #state_living{objects=Obs}=State) ->
  do_remove_object(Ob, State, Obs, []).

-spec do_remove_object(em_object:object(), #state_living{}, [em_object:object()],
                       [em_object:object()]) ->
        {ok, #state_living{}} | {{error, not_found}, #state_living{}}.
do_remove_object(_Ob, State, [], _Searched) ->
  {{error, not_found}, State};
do_remove_object(Ob, State, [Ob|Obs], Searched) ->
  {ok, State#state_living{objects=Obs ++ Searched}};
do_remove_object(Ob, State, [NoMatch|Obs], Searched) ->
  do_remove_object(Ob, State, Obs, [NoMatch|Searched]).

%% Load

-spec do_load(#state_living{}) -> {ok, #state_living{}} | {error, not_found}.
do_load(#state_living{name=Name}=State) ->
  load_living(Name, State).

-spec load_living(file_path(), #state_living{}) ->
        {ok, #state_living{}} | {error, not_found}.
load_living(Name, State) ->
  ?'log-info'("loading living: ~s",
            ['lmud-filestore':'living-file'(Name)]),
  case 'lmud-filestore':'read'("livings", Name) of
      {ok, Data} ->
      NewState = update_living(Data, State),
      {ok, NewState};
    {error, _Reason} ->
      {error, not_found}
  end.

-spec update_living([{any(), any()}], #state_living{}) -> #state_living{}.
update_living([], State) ->
  State;
update_living([{desc, Desc}|Data], State) ->
  update_living(Data, State#state_living{desc=Desc});
update_living([{room, Room}|Data], State) ->
  {ok, RoomPid} = em_room_mgr:get_room(Room),
  update_living(Data, State#state_living{room=RoomPid});
update_living([{objects, ObList}|Data], State) ->
  update_living(Data, State#state_living{objects=em_object:load_obs(ObList)});
update_living([_Other|Data], State) ->
  update_living(Data, State).

-spec do_save(#state_living{}) -> {ok, #state_living{}} | {error, any()}.
do_save(#state_living{name=Name}=State) ->
  Data = save_living(State),
  ?'log-debug'("serialised living data: ~s", [Data]),
  case 'lmud-filestore':write("livings", Name, Data) of
    ok ->
      {ok, State};
    {error, Reason} ->
      {error, file:format_error(Reason)}
  end.

-spec save_living(#state_living{}) -> string().
save_living(State) ->
    Data = 'lmud-datamodel':living(State),
  ?'log-debug'("saving living: ~p", [Data]),
  'lmud-filestore':serialise(Data).
                                 
