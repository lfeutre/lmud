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
         cmd_who/2]).

-record(state, {name, room, client}).


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
  % Needed so that we can do cleanup in terminate() when shutting down
  process_flag(trap_exit, true),
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

terminate(_Reason, #state{room=Room}) ->
  % cleanup, in case we crashed..
  em_game:logout(self()),
  em_room:leave(Room, self()),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Internal functions

parse(Line, #state{client={_,Out}}=State) ->
  [Cmd|Args] = string:tokens(Line, " "),
  try list_to_existing_atom("cmd_" ++ Cmd) of
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
      em_conn:print(Out, "I don't understand what you mean by:~n  ~s~n", [Line]),
      {ok, State}
  end.


%% Game commands

cmd_quit(_Args, #state{name=Name, client={_,Out}, room=Room}=State) ->
  em_conn:print(Out, "Goodbye!\n"),
  em_game:print_except(self(), "[Notice] ~s has logged out.~n", [Name]),
  em_room:print_except(Room, self(), "~s has left.~n", [Name]),
  ok = em_game:logout(self()),
  {stop, State}.

cmd_look(_Args, #state{client={_,Out},room=Room}=State) ->
  Desc = em_room:describe_except(Room, self()),
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
  em_conn:print(Out, "You leave " ++ Dir ++ ".\n\n"),
  Me = self(),
  NotMe = fun(Liv) -> Liv =/= Me end,
  em_room:print_while(Room, NotMe, "~s leaves ~s.~n", [Name, Dir]),
  em_room:leave(Room, self()),
  em_room:enter(Dest, self()),
  em_room:print_while(Dest, NotMe, "~s arrives.~n", [Name]),
  {ok, NewState} = cmd_look([], State#state{room=Dest}),
  NewState.

cmd_emote(Args, #state{name=Name,client={_,Out}, room=Room}=State) ->
  Text = string:join(Args, " "),
  Me = self(),
  Pred = fun(Liv) -> Liv =/= Me end, 
  em_room:print_while(Room, Pred, "~s ~s~n", [Name, Text]),
  em_conn:print(Out, "~s ~s~n", [Name, Text]),
  {ok, State}.

cmd_say([FirstWord|Rest], #state{name=Name,client={_,Out}, room=Room}=State) ->
  Text = string:join([em_text:capitalize(FirstWord)|Rest], " "),
  Me = self(),
  Pred = fun(Liv) -> Liv =/= Me end, 
  em_room:print_while(Room, Pred, "~s says, \"~s\"~n", [Name, Text]),
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

