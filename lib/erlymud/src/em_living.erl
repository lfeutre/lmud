-module(em_living).

-behaviour(gen_server).

%% API
-export([start_link/3, start/3, stop/1, cmd/2, print/2, print/3]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

%% game commands
-export([cmd_look/2, cmd_north/2, cmd_east/2, cmd_south/2, cmd_west/2,
         cmd_go/2, cmd_quit/2, cmd_emote/2, cmd_say/2, cmd_tell/2, 
         cmd_who/2, cmd_get/2, cmd_drop/2, cmd_inv/2, cmd_glance/2]).

-record(state, {name, room, client, objects=[]}).


%% API functions

start_link(Name, Room, Client) ->
  gen_server:start_link(?MODULE, [Name, Room, Client], []).

start(Name, Room, Client) ->
  gen_server:start(?MODULE, [Name, Room, Client], []).

cmd(Pid, Line) ->
  gen_server:call(Pid, {cmd, Line}).

print(Pid, Format) ->
  gen_server:call(Pid, {print, Format}).

print(Pid, Format, Args) ->
  gen_server:call(Pid, {print, Format, Args}).

stop(Pid) ->
  gen_server:cast(Pid, stop).


% gen_server callbacks

init([Name, Room, Client]) ->
  {ok, #state{name=Name, room=Room, client=Client}}.

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
  {reply, ok, State}.

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
  Line = [" ", em_object:a_short(Ob)],
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
  Obs = em_room:get_objects(Room),
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
      do_get(Name, Obs, State)
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

cmd_look(_Args, #state{client={_,Out},room=Room}=State) ->
  Desc = em_room:looking(Room, self()),
  em_conn:print(Out, Desc),
  {ok, State}.

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

