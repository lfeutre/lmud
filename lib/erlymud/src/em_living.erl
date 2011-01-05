-module(em_living).

-behaviour(gen_server).

%% API
-export([start_link/2, start/2, stop/1, 
         get_name/1, 
         get_room/1, set_room/2,
         set_long/2, long/1,
         cmd/2, print/2, print/3,
         load/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

%% game commands
-export([cmd_look/2, cmd_north/2, cmd_east/2, cmd_south/2, cmd_west/2,
         cmd_go/2, cmd_quit/2, cmd_emote/2, cmd_say/2, cmd_tell/2, 
         cmd_who/2, cmd_get/2, cmd_drop/2, cmd_inv/2, cmd_glance/2,
         cmd_save/2, cmd_setlong/2]).

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


% gen_server callbacks

init([Name, Client]) ->
  {ok, #state{name=Name, client=Client}}.

handle_call(get_name, _From, #state{name=Name}=State) ->
  {reply, Name, State};
handle_call(get_room, _From, #state{room=Room}=State) ->
  {reply, Room, State};
handle_call({set_room_pid, Room}, _From, State) ->
  {reply, ok, State#state{room=Room}};
handle_call({set_room_str, RoomStr}, _From, State) ->
  {ok, Room} = em_room_mgr:get_room(RoomStr),
  {reply, ok, State#state{room=Room}};
handle_call({set_long, Long}, _From, State) ->
  {reply, ok, State#state{long=Long}};
handle_call(long, _From, #state{long=Long}=State) ->
  {reply, Long, State};
handle_call({cmd, Line}, _From, State) ->
  case parse(Line, State) of
    {ok, NewState} ->
      {reply, ok, NewState};
    {stop, NewState} ->
      {stop, normal, NewState}
  end;
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

parse(Line, #state{client={_,Out}}=State) ->
  [Cmd|Args] = string:tokens(Line, " "),
  try list_to_existing_atom("cmd_" ++ string:to_lower(Cmd)) of
    Fun ->
      case apply(?MODULE, Fun, [Args, State]) of
        {ok, NewState} ->
          {ok, NewState};
        {stop, NewState} ->
          {stop, NewState};
        {error, Reason} ->
          em_conn:print(Out, Reason),
          {ok, State};
        Other ->
          em_conn:print(Out, "Error occurred while processing '~s':~n~p~n", 
            [Line, Other]),
          {ok, State}
      end
  catch
    error:badarg ->
      em_conn:print(Out, "I don't understand what you mean by '~s'~n", [Line]),
      {ok, State}
  end.


%% Game commands

cmd_inv(_Args, #state{objects=Obs}=State) ->
  {ok, do_inv(Obs, State)}.

do_inv([], State) ->
  do_print("You're not carrying anything.\n", State),
  State;
do_inv(Obs, State) ->
  do_print("You're carrying:\n", State),
  do_print(desc_inv(Obs, []), State),
  State.

desc_inv([], Result) ->
  Result;
desc_inv([Ob|Obs], Result) ->
  Line = [" ", em_object:a_short(Ob), "\n"],
  desc_inv(Obs, [Result, Line]).

cmd_drop([], State) ->
  do_print("Drop what?\n", State),
  {ok, State};
cmd_drop([Id|_Args], #state{objects=Obs}=State) ->
  {ok, do_drop(Id, Obs, State)}.

do_drop(_Id, [], State) ->
  do_print("You don't have anything like that.\n", State),
  State;
do_drop(Id, [Ob|Obs], #state{name=Name, room=Room, objects=Objects}=State) ->
  case em_object:has_id(Ob, Id) of
    true ->
      TheShort = em_object:the_short(Ob),
      do_print("You drop ~s.\n", [TheShort], State),
      em_room:add_object(Room, Ob),
      em_room:print_except(Room, self(), "~s drops ~s.~n", [Name, TheShort]),
      State#state{objects=lists:delete(Ob, Objects)};
    false ->
      do_drop(Name, Obs, State)
  end.

cmd_get([], State) ->
  do_print("Get what?\n", State),
  {ok, State};
cmd_get([Id|_Args], #state{room=Room}=State) ->
  Obs = lists:filter(fun(Ob) -> not em_object:is_attached(Ob) end,
                     em_room:get_objects(Room)),
  {ok, do_get(Id, Obs, State)}.

do_get(_Id, [], State) ->
  do_print("There's no such thing here.\n", State),
  State;
do_get(Id, [Ob|Obs], #state{name=Name, room=Room, objects=Objects}=State) ->
  case em_object:has_id(Ob, Id) of
    true ->
      TheShort = em_object:the_short(Ob),
      do_print("You take ~s.\n", [TheShort], State),
      em_room:remove_object(Room, Ob),
      em_room:print_except(Room, self(), "~s takes ~s.~n", [Name, TheShort]),
      State#state{objects=[Ob|Objects]};
    false ->
      do_get(Id, Obs, State)
  end.

do_print(Format, State) ->
  do_print(Format, [], State).
do_print(Format, Args, #state{client={_,Out}}) ->
  em_conn:print(Out, Format, Args).
    
cmd_quit(_Args, #state{name=Name, client={_,Out}, room=Room}=State) ->
  em_conn:print(Out, "Goodbye!\n"),
  em_room:print_except(Room, self(), "~s has left.~n", [Name]),
  ok = em_game:logout(self()),
  {stop, State}.

cmd_glance(_Args, #state{client={_,Out},room=Room}=State) ->
  Desc = em_room:describe_except(Room, self()),
  em_conn:print(Out, Desc),
  {ok, State}.

cmd_look([], #state{client={_,Out},room=Room}=State) ->
  Desc = em_room:looking(Room, self()),
  em_conn:print(Out, Desc),
  {ok, State};
cmd_look([Id|_Args], #state{room=Room}=State) ->
  Obs = em_room:get_objects(Room),
  case do_look_ob(string:to_lower(Id), Obs, State) of
    {ok, State} -> {ok, State};
    {error, not_found} -> 
      People = lists:delete(self(), em_room:get_people(Room)),
      case do_look_liv(string:to_lower(Id), People, State) of
        {ok, State} -> {ok, State};
        {error, not_found} ->
          do_print("There's no such thing here.\n", State),
          {ok, State}
      end
  end.

do_look_ob(_Id, [], _State) ->
  {error, not_found};
do_look_ob(Id, [Ob|Obs], State) ->
  case em_object:has_id(Ob, Id) of
    true ->
      Long = em_object:long(Ob),
      do_print("~s\n", [em_text:wrapline(Long, 78)], State),
      {ok, State};
    false ->
      do_look_ob(Id, Obs, State)
  end.

do_look_liv(_Id, [], _State) ->
  {error, not_found};
do_look_liv(Id, [Liv|People], State) ->
  case string:to_lower(em_living:get_name(Liv)) of
    Id ->
      Long = em_living:long(Liv),
      do_print("~s\n", [em_text:wrapline(Long, 78)], State),
      {ok, State};
    _Other ->
      do_look_liv(Id, People, State)
  end.

  
cmd_north(_Args, State) ->
  cmd_go(["north"], State).
cmd_east(_Args, State) ->
  cmd_go(["east"], State).
cmd_south(_Args, State) ->
  cmd_go(["south"], State).
cmd_west(_Args, State) ->
  cmd_go(["west"], State).

cmd_go([Dir|_Args], #state{room=Room}=State) ->
  {ok, do_go(em_room:get_exit(Room, Dir), State)}.

do_go({error, not_found}, #state{client={_,Out}}=State) ->
  em_conn:print(Out, "You can't go in that direction.\n"),
  State;
do_go({ok, {Dir, Dest}}, #state{name=Name, client={_,Out}, room=Room}=State) ->
  {ok, DestRoom} = em_room_mgr:get_room(Dest),
  em_conn:print(Out, "You leave " ++ Dir ++ ".\n\n"),
  em_room:print_except(Room, self(), "~s leaves ~s.~n", [Name, Dir]),
  em_room:leave(Room, self()),
  em_room:enter(DestRoom, self()),
  em_room:print_except(DestRoom, self(), "~s arrives.~n", [Name]),
  {ok, NewState} = cmd_glance([], State#state{room=DestRoom}),
  NewState.

cmd_emote(Args, #state{name=Name,client={_,Out}, room=Room}=State) ->
  Text = string:join(Args, " "),
  em_room:print_except(Room, self(), "~s ~s~n", [Name, Text]),
  em_conn:print(Out, "~s ~s~n", [Name, Text]),
  {ok, State}.

cmd_say([FirstWord|Rest], #state{name=Name,client={_,Out}, room=Room}=State) ->
  Text = string:join([em_text:capitalize(FirstWord)|Rest], " "),
  em_room:print_except(Room, self(), "~s says, \"~s\"~n", [Name, Text]),
  em_conn:print(Out, "You say, \"~s\"~n", [Text]),
  {ok, State}.

cmd_tell([Who,FirstWord|Rest], #state{name=Name,client={_,Out}}=State) ->
  case em_game:lookup_user(Who) of
    {error, not_found} ->
      em_conn:print(Out, "There's no such user.\n");
    {ok, {Name, _}} ->
      em_conn:print(Out, "Talking to yourself, huh?\n");
    {ok, {OtherName, OtherUser}} ->
      Text = string:join([em_text:capitalize(FirstWord)|Rest], " "),
      print(OtherUser, "~s tells you, \"~s\"~n", [Name, Text]),
      em_conn:print(Out, "You tell ~s, \"~s\"~n", [OtherName, Text])
  end,
  {ok, State}.

cmd_who(_Args, #state{client={_,Out}}=State) ->
  em_conn:print(Out, ["Users:\n",
    [[" ", Name, "\n"] || {Name, _Pid} <- em_game:get_users()]]),
  {ok, State}.

cmd_save(_Args, #state{client={_,Out}}=State) ->
  em_conn:print(Out, "Saving..\n"),
  case do_save(State) of
    {ok, NewState} ->
      {ok, NewState};
    {error, Reason} ->
      em_conn:print(Out, "Error: ~s\n", [Reason]),
      {ok, State}
  end.

cmd_setlong(Args, State) ->
  {ok, State#state{long=string:join(Args, " ")}}.

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
  
