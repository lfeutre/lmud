%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Game server.
%%% Keeps track of some global state in the game, currently a list of the
%%% logged-on users.
%%% @end
%%% =========================================================================
-module(lmud_game).

-behaviour(gen_server).

%% API
-export([start_link/0, start/0,
         connected_since/1, 'connected-since'/1,
         data_dir/0,
         get_users/0, get_user_names/0,
         lookup_user/1, lookup_user_pid/1,
         get_characters/0, 'get-characters'/0, get_character_names/0,
         lookup_character/1, 'lookup-character'/1,
         member_since/1, 'member-since'/1,
         login/1, incarnate/1, logout/1,
         print_except/3, print_while/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("logjam/include/logjam.hrl").
-include("apps/lmud/include/state.hrl").

-define(SERVER, ?MODULE).


%% ==========================================================================
%% Type Specifications
%% ==========================================================================
-include("apps/lmud/include/types.hrl").

-type user() :: {string(), pid()}.
-type users() :: [user()].
-type character() :: {string(), pid()}.
-type characters() :: [character()].

-export_type([user/0, users/0]).


%% ==========================================================================
%% API Functions
%% ==========================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
  gen_server:start({local, ?SERVER}, ?MODULE, [], []).

connected_since(Name) ->
  gen_server:call(?SERVER, {'connected-since', Name}).

'connected-since'(Name) ->
  gen_server:call(?SERVER, {'connected-since', Name}).

%% @doc Return path to the data directory.
-spec data_dir() -> file_path().
data_dir() ->
  'lmud-util':'data-dir'().

%% @doc Return a list of {Name, UserPid} tuples with all logged in users.
-spec get_characters() -> characters().
get_characters() ->
  gen_server:call(?SERVER, 'get-characters').

'get-characters'() ->
  gen_server:call(?SERVER, 'get-characters').

get_character_names() ->
  [[" ", Name, "\n"] || {Name, _Pid} <- get_characters()].

-spec get_users() -> users().
get_users() ->
  gen_server:call(?SERVER, get_users).

get_user_names() ->
  [[" ", Name, "\n"] || {Name, _Pid} <- get_users()].

%% @doc Log in a user to the game. Returns an error if the user is already
%% logged in.
login(User) ->
  ?'log-debug'("logging in user ~p", [User]),
  gen_server:call(?SERVER, {login, User}).

%% @doc Incarnate a character, putting them in the room where they last saved.
-spec incarnate(lmud_character:pid_type()) -> ok.
incarnate(Character) ->
  gen_server:call(?SERVER, {incarnate, Character}).

%% @doc Log out the specified user. Returns an error if user isn't logged on.
logout(User) ->
  gen_server:call(?SERVER, {logout, User}).

lookup_character(Name) ->
  gen_server:call(?SERVER, {'lookup-character', Name}).

'lookup-character'(Name) ->
  gen_server:call(?SERVER, {'lookup-character', Name}).

%% @doc Lookup a user by name, return the {Name, UserPid} tuple.
lookup_user(Name) ->
  gen_server:call(?SERVER, {lookup_user, Name}).

%% @doc Lookup a user by pid, return the {Name, UserPid} tuple.
lookup_user_pid(Pid) ->
  gen_server:call(?SERVER, {lookup_user_pid, Pid}).

member_since(Pid) ->
  gen_server:call(?SERVER, {'member-since', Pid}).

'member-since'(Pid) ->
  gen_server:call(?SERVER, {'member-since', Pid}).

%% @doc Print something to all users except the specified UserPid.
print_except(User, Format, Args) ->
  gen_server:call(?SERVER, {print_except, User, Format, Args}).

%% @doc Print something to all users that satisfy the predicate fun.
print_while(Pred, Format, Args) ->
  gen_server:call(?SERVER, {print_while, Pred, Format, Args}).


%% ==========================================================================
%% gen_server callbacks
%% ==========================================================================

init([]) ->
  process_flag(trap_exit, true),
  {ok, #state_game{}}.

handle_call('get-characters', _From, #state_game{characters=Chars}=State) ->
  {reply, Chars, State};
handle_call(get_users, _From, #state_game{users=Users}=State) ->
  {reply, Users, State};
handle_call({login, User}, _From, State) ->
  ?'log-debug'("logging in user ~p ...", [User]),
  {Result, NewState} = do_login(User, State),
  {reply, Result, NewState};
handle_call({'connected-since', Name}, _From, #state_game{characters=Chars}=State) when is_list(Name) ->
  Result = case do_lookup(Name, Chars) of
             {ok, {_, Pid}} -> {ok, lmud_character:connected_since(Pid)};
             {error, _} -> {error, no_character_data}
           end,
  {reply, Result, State};
handle_call({incarnate, Character}, _From, State) ->
  {Result, NewState} = do_incarnate(Character, State),
  {reply, Result, NewState};
handle_call({logout, User}, _From, State) ->
  ?'log-debug'("logging out user ..."),
  {Result, NewState} = do_logout(User, State),
  {reply, Result, NewState};
handle_call({'lookup-character', Name}, _From, #state_game{characters=Chars}=State) when is_list(Name) ->
  {reply, do_lookup(Name, Chars), State};
handle_call({lookup_user, Name}, _From, #state_game{users=Users}=State) when is_list(Name) ->
  {reply, do_lookup(Name, Users), State};
handle_call({lookup_user_pid, Pid}, _From, #state_game{users=Users}=State) when is_pid(Pid) ->
  Result = do_lookup_user_pid(Pid, Users),
  {reply, Result, State};
handle_call({print_except, User, Format, Args}, _From, State) ->
  do_print_except(State#state_game.users, User, Format, Args),
  {reply, ok, State};
handle_call({print_while, Pred, Format, Args}, _From, State) ->
  do_print_while(State#state_game.users, Pred, Format, Args),
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

% If user dies, remove from state; die on other EXIT signals
handle_info({'EXIT', From, Reason}, #state_game{users=Users}=State) ->
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

do_login(User, #state_game{users=Users}=State) ->
  Name = 'lmud-user':name(User),
  %%NameKey = string:to_upper(Name), % Re-enable when all internal use is standardised on upper case
  NameKey = Name,
  ?'log-debug'("trying to log in user ~p (~p)", [Name, User]),
  ?'log-debug'("users in game: ~p", [Users]),
  case lists:keyfind(NameKey, 1, Users) of
    {_Name, _User} ->
      {{error, user_exists}, State};
    false ->
      link(User),
      do_print_except(Users, User, color:blue("[Notice]") ++ " ~s has logged in.~n", [Name]),
      {ok, State#state_game{users=[{NameKey, User}|Users]}}
  end.

-spec do_incarnate(lmud_character:pid_type(), #state_game{}) -> {ok, #state_game{}}.
do_incarnate(Character, #state_game{characters=Characters}=State) ->
  Name = lmud_character:name(Character),
  ?'log-debug'("incarnating character ~s (~p)", [Name, Character]),
  Room = case lmud_character:get_room(Character) of
           undefined ->
             {ok, StartRoom} = lmud_room_mgr:get_room("room1"), % TODO: let's not hard-code this ... put in config
             lmud_character:set_room(Character, StartRoom),
             StartRoom;
           LoadedRoom ->
             LoadedRoom
         end,
  do_print_except(Characters, Character, color:blue("[Notice]") ++ " ~s has joined the game.~n", [Name]),
  lmud_room:enter(Room, Character),
  lmud_room:print_except(yellowb, Room, Character, "~n~s arrives.~n", [Name]),
  {ok, State#state_game{characters=[{Name, Character}|Characters]}}.

%% @doc Log out a user. Do NOT actually touch the User process here, it might
%% have crashed when we call do_logout(). Or could it really, since we link?!
do_logout(User, #state_game{users=Users, characters=Characters}=State) ->
  ?'log-debug'("Users: ~p", [Users]),
  Character = 'lmud-user':'get-character'(User),
  NewState = case lists:keyfind(Character, 2, Characters) of
    {_, Character} ->
      State#state_game{characters=lists:keydelete(Character, 2, Characters)};
    false ->
      State
  end,
  case lists:keyfind(User, 2, Users) of
    {UserName, User} ->
      unlink(User),      
      ?'log-notice'("~s has logged out", [UserName]),
      do_print_except(Users, User, "[Notice] ~s has logged out.~n", [UserName]),
      {ok, NewState#state_game{users=lists:keydelete(User, 2, Users)}};
    false ->
      ?'log-error'("Couldn't find user in game state"),
      {{error, not_found}, NewState}
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
  PrintFun = fun(Pid) -> lmud_character:print(Pid, Format, Args) end,
  lists:map(PrintFun, ToPids).

do_lookup(Name, Collection) ->
  %%NameKey = string:to_upper(Name), % TODO: enable this when EVERYTHING internal is using uppercase
  NameKey = Name,
  case lists:keyfind(NameKey, 1, Collection) of
    false -> {error, not_found};
    LookupTuple -> {ok, LookupTuple}
  end.
  
