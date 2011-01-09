%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Game server.
%%% Keeps track of some global state in the game, currently a list of the
%%% logged-on users.
%%% @end
%%% =========================================================================
-module(em_game).

-behaviour(gen_server).

%% API
-export([start_link/0, start/0, 
         get_users/0, lookup_user/1, lookup_user_pid/1,
         login/1, incarnate/1, logout/1,
         print_except/3, print_while/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(state, {users=[]}).

-define(SERVER, ?MODULE).


%% API functions

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
  gen_server:start({local, ?SERVER}, ?MODULE, [], []).

get_users() ->
  gen_server:call(?SERVER, get_users).

login(User) ->
  gen_server:call(?SERVER, {login, User}).

incarnate(Living) ->
  gen_server:call(?SERVER, {incarnate, Living}).

logout(User) ->
  gen_server:call(?SERVER, {logout, User}).

lookup_user(Name) ->
  gen_server:call(?SERVER, {lookup_user, Name}).

lookup_user_pid(Pid) ->
  gen_server:call(?SERVER, {lookup_user_pid, Pid}).

print_except(User, Format, Args) ->
  gen_server:call(?SERVER, {print_except, User, Format, Args}).

print_while(Pred, Format, Args) ->
  gen_server:call(?SERVER, {print_while, Pred, Format, Args}).

%% gen_server callbacks

init([]) ->
  process_flag(trap_exit, true),
  {ok, #state{}}.

handle_call(get_users, _From, #state{users=Users}=State) ->
  {reply, Users, State};
handle_call({login, User}, _From, State) ->
  {Result, NewState} = do_login(User, State),
  {reply, Result, NewState};
handle_call({incarnate, Living}, _From, State) ->
  {Result, NewState} = do_incarnate(Living, State),
  {reply, Result, NewState};
handle_call({logout, User}, _From, State) ->
  {Result, NewState} = do_logout(User, State),
  {reply, Result, NewState};
handle_call({lookup_user, Name}, _From, #state{users=Users}=State) 
    when is_list(Name) ->
  Result = case lists:keyfind(em_text:capitalize(Name), 1, Users) of
             false -> {error, not_found};
             UserTuple -> {ok, UserTuple}
           end,
  {reply, Result, State};
handle_call({lookup_user_pid, Pid}, _From, #state{users=Users}=State) 
    when is_pid(Pid) ->
  Result = do_lookup_user_pid(Pid, Users),
  {reply, Result, State};
handle_call({print_except, User, Format, Args}, _From, State) ->
  do_print_except(State#state.users, User, Format, Args),
  {reply, ok, State};
handle_call({print_while, Pred, Format, Args}, _From, State) ->
  do_print_while(State#state.users, Pred, Format, Args),
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

% If user dies, remove from state; die on other EXIT signals
handle_info({'EXIT', From, Reason}, #state{users=Users}=State) ->
  case do_lookup_user_pid(From, Users) of
    {ok, {_Name, User}} ->
      {ok, NewState} = do_logout(User, State),
      {noreply, NewState};
    {error, not_found} ->
      {stop, Reason, State}
  end.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Internal functions

%% User Handling

do_login(User, #state{users=Users}=State) ->
  Name = em_user:get_name(User),
  case lists:keyfind(Name, 1, Users) of
    {_Name, _User} ->
      {{error, user_exists}, State};
    false ->
      link(User),
      do_print_except(Users, User, "[Notice] ~s has logged in.~n", [Name]),
      {ok, State#state{users=[{Name, User}|Users]}}
  end.

do_incarnate(Living, State) ->
  Name = em_living:get_name(Living),
  Room = case em_living:get_room(Living) of
           undefined ->
             {ok, StartRoom} = em_room_mgr:get_room("room1"),
             em_living:set_room(Living, StartRoom),
             StartRoom;
           LoadedRoom ->
             LoadedRoom
         end,
  em_room:enter(Room, Living),
  em_room:print_except(Room, Living, "~s arrives.~n", [Name]),
  {ok, State}.

% Do NOT actually touch the User process here, it might have crashed
% when we call do_logout()
do_logout(User, #state{users=Users}=State) ->
  case lists:keyfind(User, 2, Users) of
    {Name, User} ->
      unlink(User),
      do_print_except(Users, User, "[Notice] ~s has logged out.~n", [Name]),
      {ok, State#state{users=lists:keydelete(User, 2, Users)}};
    false ->
      {{error, not_found}, State}
  end.

do_lookup_user_pid(Pid, Users) ->
  case lists:keyfind(Pid, 2, Users) of
    false -> {error, not_found};
    UserTuple -> {ok, UserTuple}
  end.

do_print_except(Users, User, Format, Args) ->
  Pred = fun(U) -> U =/= User end,
  do_print_while(Users, Pred, Format, Args).

do_print_while(Users, Pred, Format, Args) ->
  Pids = [Pid || {_User, Pid} <- Users],
  ToPids = lists:filter(Pred, Pids),
  PrintFun = fun(Pid) -> em_living:print(Pid, Format, Args) end,
  lists:map(PrintFun, ToPids).
