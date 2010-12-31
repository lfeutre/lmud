-module(em_living).

-behaviour(gen_server).

%% API
-export([start_link/3, start/3, stop/1, cmd/2, print/2, print/3]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

%% game commands
-export([cmd_look/2, cmd_north/2, cmd_east/2, cmd_south/2, cmd_west/2,
         cmd_quit/2, cmd_say/2, cmd_tell/2, cmd_who/2]).

-record(state, {name, room, client}).


%% API functions

start_link(Name, Room, Client) ->
  gen_server:start_link(?MODULE, [Name, Room, Client], []).

start(Name, Room, Client) ->
  gen_server:start(?MODULE, [Name, Room, Client], []).

cmd(Pid, Line) ->
  gen_server:call(Pid, {cmd, Line}).

print(Pid, Format) ->
  gen_server:cast(Pid, {print, Format}).

print(Pid, Format, Args) ->
  gen_server:cast(Pid, {print, Format, Args}).

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
  end.

handle_cast({print, Format}, State) ->
  em_conn:print(State#state.client, Format),
  {noreply, State};
handle_cast({print, Format, Args}, State) ->
  em_conn:print(State#state.client, Format, Args),
  {noreply, State};
handle_cast(stop, #state{client=Client}=State) ->
  em_conn:print(Client, "living(): stopping.~n"),
  {stop, normal, ok, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Internal functions

parse(Line, State) ->
  [Cmd|Args] = string:tokens(Line, " "),
  try list_to_existing_atom("cmd_" ++ Cmd) of
    Fun ->
      case apply(?MODULE, Fun, [Args, State]) of
        {ok, NewState} ->
          {ok, NewState};
        {stop, NewState} ->
          {stop, NewState};
        {error, Reason} ->
          print(self(), Reason),
          {ok, State};
        Other ->
          print(self(), "Error occurred while processing '~s':~n~p~n", 
            [Line, Other]),
          {ok, State}
      end
  catch
    error:badarg ->
      print(self(), "I don't understand what you mean by:~n  ~s~n", [Line]),
      {ok, State}
  end.


%% Game commands

cmd_quit(_Args, State) ->
  ok = em_game:logout(self()),
  {stop, State}.

cmd_look(_Args, #state{room=Room}=State) ->
  Desc = em_room:describe(Room),
  Exits = em_room:get_exits(Room),
  ExitLine = lists:foldl(fun({Dir,_}, Str) -> [Str, Dir, " "] end, "", Exits), 
  print(self(), [Desc, "Exits: ", ExitLine, "\n"]),
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

do_go({error, not_found}, State) ->
  print(self(), "You can't go in that direction.\n"),
  State;
do_go({ok, {Dir, Dest}}, #state{room=Room}=State) ->
  print(self(), "You leave " ++ Dir ++ ".\n"),
  em_room:leave(Room, self()),
  em_room:enter(Dest, self()),
  {ok, NewState} = cmd_look([], State#state{room=Dest}),
  NewState.

cmd_say([FirstWord|Rest], #state{name=Name, room=Room}=State) ->
  Text = string:join([em_text:capitalize(FirstWord)|Rest], " "),
  Me = self(),
  Pred = fun(Liv) -> Liv =/= Me end, 
  em_room:print_while(Room, Pred, "~s says, \"~s\"~n", [Name, Text]),
  print(self(), "You say, \"~s\"~n", [Text]),
  {ok, State}.

cmd_tell([Who,FirstWord|Rest], #state{name=Name}=State) ->
  case em_game:lookup_user(Who) of
    {error, not_found} ->
      print(self(), "There's no such user.\n");
    {ok, {Name, _}} ->
      print(self(), "Talking to yourself, huh?\n");
    {ok, {OtherName, OtherUser}} ->
      Text = string:join([em_text:capitalize(FirstWord)|Rest], " "),
      print(OtherUser, "~s tells you, \"~s\"~n", [Name, Text]),
      print(self(), "You tell ~s, \"~s\"~n", [OtherName, Text])
  end,
  {ok, State}.

cmd_who(_Args, State) ->
  print(self(), ["Users:\n",
    [[" ", Name, "\n"] || {Name, _Pid} <- em_game:get_users()]]),
  {ok, State}.

