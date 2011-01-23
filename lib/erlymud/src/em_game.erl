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
         data_dir/0,
         get_users/0, lookup_user/1, lookup_user_pid/1,
         login/1, incarnate/1, logout/1,
         print_except/3, print_while/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(state, {users=[]::users()}).

-define(SERVER, ?MODULE).


%% ==========================================================================
%% Type Specifications
%% ==========================================================================
-include("types.hrl").
-type user() :: {string(), pid()}.
-type users() :: [user()].
-type user_pred() :: fun((em_user:user_pid()) -> true|false).


%% ==========================================================================
%% API Functions
%% ==========================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
  gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%% @doc Return path to the data directory.
-spec data_dir() -> file_path().
data_dir() ->
  filename:join([code:lib_dir(erlymud), "..", "..", "data"]).

%% @doc Return a list of {Name, UserPid} tuples with all logged in users.
-spec get_users() -> users().
get_users() ->
  gen_server:call(?SERVER, get_users).

%% @doc Log in a user to the game. Returns an error if the user is already
%% logged in.
-spec login(em_user:user_pid()) -> ok | {error, user_exists}.
login(User) ->
  gen_server:call(?SERVER, {login, User}).

%% @doc Incarnate a living, putting them in the room where they last saved.
-spec incarnate(em_living:living_pid()) -> ok.
incarnate(Living) ->
  gen_server:call(?SERVER, {incarnate, Living}).

%% @doc Log out the specified user. Returns an error if user isn't logged on.
-spec logout(em_user:user_pid()) -> ok | {error, not_found}.
logout(User) ->
  gen_server:call(?SERVER, {logout, User}).

%% @doc Lookup a user by name, return the {Name, UserPid} tuple.
-spec lookup_user(em_user:user_name()) -> {ok, user()} | {error, not_found}.
lookup_user(Name) ->
  gen_server:call(?SERVER, {lookup_user, Name}).

%% @doc Lookup a user by pid, return the {Name, UserPid} tuple.
-spec lookup_user_pid(em_user:user_pid()) -> {ok, user()} | {error, not_found}.
lookup_user_pid(Pid) ->
  gen_server:call(?SERVER, {lookup_user_pid, Pid}).

%% @doc Print something to all users except the specified UserPid.
-spec print_except(em_user:user_pid(), iolist(), list()) -> ok.
print_except(User, Format, Args) ->
  gen_server:call(?SERVER, {print_except, User, Format, Args}).

%% @doc Print something to all users that satisfy the predicate fun.
-spec print_while(user_pred(), iolist(), list()) -> ok.
print_while(Pred, Format, Args) ->
  gen_server:call(?SERVER, {print_while, Pred, Format, Args}).


%% ==========================================================================
%% gen_server callbacks
%% ==========================================================================

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


%% ==========================================================================
%% Internal functions
%% ==========================================================================

%% User Handling

-spec do_login(em_user:user_pid(), #state{}) -> 
        {ok, #state{}}|{{error, user_exists}, #state{}}.
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

-spec do_incarnate(em_living:living_pid(), #state{}) -> {ok, #state{}}.
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

%% @doc Log out a user. Do NOT actually touch the User process here, it might 
%% have crashed when we call do_logout(). Or could it really, since we link?!
-spec do_logout(em_user:user_pid(), #state{}) -> 
  {ok, #state{}}|{{error, not_found}, #state{}}.
do_logout(User, #state{users=Users}=State) ->
  case lists:keyfind(User, 2, Users) of
    {Name, User} ->
      unlink(User),
      do_print_except(Users, User, "[Notice] ~s has logged out.~n", [Name]),
      {ok, State#state{users=lists:keydelete(User, 2, Users)}};
    false ->
      {{error, not_found}, State}
  end.

-spec do_lookup_user_pid(em_user:user_pid(), users()) -> 
        {ok, user()}|{error, not_found}.
do_lookup_user_pid(Pid, Users) ->
  case lists:keyfind(Pid, 2, Users) of
    false -> {error, not_found};
    UserTuple -> {ok, UserTuple}
  end.

-spec do_print_except(users(), em_user:user_pid(), iolist(), list()) -> list().
do_print_except(Users, User, Format, Args) ->
  Pred = fun(U) -> U =/= User end,
  do_print_while(Users, Pred, Format, Args).

-spec do_print_while(users(), user_pred(), iolist(), list()) -> list().
do_print_while(Users, Pred, Format, Args) ->
  Pids = [Pid || {_User, Pid} <- Users],
  ToPids = lists:filter(Pred, Pids),
  PrintFun = fun(Pid) -> em_living:print(Pid, Format, Args) end,
  lists:map(PrintFun, ToPids).
