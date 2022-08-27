%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Login request handler.
%%% Will handle user login / new user creation, before passing the user on
%%% to the in-game request handler.
%%% @end
%%% =========================================================================
-module(msh_login).

%% API
-export([welcome/1, login/3]).

-include_lib("logjam/include/logjam.hrl").

-include("apps/mudshell/include/request.hrl").

-include("apps/lmud/include/state.hrl").
-include("apps/lmud/include/types.hrl").


%% ==========================================================================
%% API Functions
%% ==========================================================================

-spec welcome(mn_conn:conn_pid()) -> ok.
welcome(Conn) ->
  mn_conn:print(Conn, "\nWelcome to:\n"
    ++ 'lmud-config':banner() ++ "\nAn "
    ++ 'lmud-config':description() ++ ", v"
    ++ 'lmud-config':version() ++ "\n\n"
    ++ 'lmud-config':'login-instructions'() ++ "\n\n"),
  mn_conn:print(Conn, "Login: ").

%% Got a username, do something with it
-spec login(any(), string(), req()) -> req_ok() | req_link().
login(got_user, "", #req{conn=Conn}=Req) ->
  ?'log-debug'("got invalid user name"),
  mn_conn:print(Conn, "Invalid username.\n\n"),
  mn_conn:print(Conn, "Login: "),
  ?req_next(login, [got_user]);
login(got_user, UserName, #req{conn=Conn}=Req) ->
  case mudstore:load("users", UserName) of
    {ok, Settings} ->
      mn_conn:print(Conn, "Password: "),
      mn_conn:echo_off(Conn),
      ?req_next(login, [{got_password, Settings, UserName}]);
    {error, _Reason} ->
      mn_conn:print(Conn, "New user account \"~s\".\n", [UserName]),
      mn_conn:print(Conn, "Are you sure (Y/N)? "),
      ?req_next(login, [{new_user_confirm, UserName}])
  end;
%% It's a new user, confirm if name is correct
login({new_user_confirm, Name}, YesNo, #req{conn=Conn}=Req) ->
  ?'log-debug'("creating new user for ~p ...", [Name]),
  case string:to_lower(YesNo) of
    [$y|_Rest] ->
      mn_conn:print(Conn, "\nPick a password: "),
      mn_conn:echo_off(Conn),
      ?req_next(login, [{new_user_pw, Name}]);
    _Other ->
      mn_conn:print(Conn, "\nLogin: "),
      ?req_next(login, [got_user])
  end;
%% Pick a password for new user account
login({new_user_pw, Name}, "", #req{conn=Conn}=Req) ->
  mn_conn:print(Conn, "Invalid password.\n\n"),
  mn_conn:print(Conn, "Pick a password: "),
  ?req_next(login, [{new_user_pw, Name}]);
login({new_user_pw, Name}, Password, #req{conn=Conn}=Req) ->
  mn_conn:print(Conn, "\nRepeat the password: "),
  ?req_next(login, [{new_user_pw_confirm, Name, Password}]);
%% Confirm the password
login({new_user_pw_confirm, Name, Password}, Password, #req{conn=Conn}=Req) ->
  mn_conn:echo_on(Conn),
  UserData = mudstore:serialise(#state_user{id=lmud:id(), name=Name, password='lmud-crypto':hash(Password)}),
  mudstore:dump("users", Name, UserData),
  CharDesc = lists:flatten(io_lib:format("~s looks fairly ordinary; maybe they should update their description?", [Name])),
  CharData = mudstore:serialise(#state_character{id=lmud:id(), name=Name, desc=CharDesc}),
  mudstore:dump("characters", Name, CharData),
  mn_conn:print(Conn, ["\nWelcome, ", Name, "!\n\n"]),
  do_login(Name, Req);
login({new_user_pw_confirm, Name, _Password}, _WrongPassword, #req{conn=Conn}=Req) ->
  mn_conn:print(Conn, "Passwords don't match, please try again.\n\n"),
  mn_conn:print(Conn, "Pick a password: "),
  ?req_next(login, [{new_user_pw, Name}]);
%% Existing user; load file and compare passwords, log in if correct
login({got_password, Settings, Name}, Password, #req{conn=Conn}=Req) ->
  ?'log-debug'("attempting login for ~p ...", [Name]),
  mn_conn:echo_on(Conn),
  CryptPw = 'lmud-crypto':hash(Password),
  case lists:keyfind(password, 1, Settings) of
    false ->
      mn_conn:print(Conn, "Error: Invalid user file.\n\n"),
      mn_conn:print(Conn, "Login: "),
      ?req_next(login, [got_user]);
    {password, CryptPw} ->
      do_login(Name, Req);
    {password, _Other} ->
      mn_conn:print(Conn, "Invalid password.\n\n"),
      mn_conn:print(Conn, "Login: "),
      ?req_next(login, [got_user])
  end.

-spec do_login(string(), req()) -> req_ok() | req_link().
do_login(Name, #req{conn=Conn}=Req) ->
  ?'log-debug'("attempting login for ~p ...", [Name]),
  {ok, UserPid} = 'lmud-user-sup':start_child(Name, Conn),
  ?'log-debug'("created supervised child: ~p ...", [UserPid]),
  link(UserPid),
  ?'log-debug'("loading user ~p (~p) ...", [Name, UserPid]),
  ?'log-debug'("state before: ~p", ['lmud-user':state(UserPid)]),
  ok = 'lmud-user':load(UserPid),
  ?'log-debug'("state after: ~p", ['lmud-user':state(UserPid)]),
  case lmud_game:login(UserPid) of
    ok ->
      {ok, Character} = 'lmud-character-sup':start_child(Name, {UserPid, Conn}),
      link(Character),
      ok = lmud_character:load(Character),
      {ok, NewReq} = do_incarnate(Req#req{user=UserPid, character=Character}),
      'lmud-user':'set-character'(UserPid, Character),
      unlink(Character),
      unlink(UserPid),
      ?req_next_and_link(msh_parser, parse, [], NewReq);
    {error, user_exists} ->
      unlink(UserPid),
      exit(UserPid, user_exists),
      mn_conn:print(Conn, "User already logged in, try again.\n\n"),
      mn_conn:print(Conn, "Login: "),
      ?req_next(login, [got_user])
  end.

-spec do_incarnate(req()) -> {ok, req()}.
do_incarnate(#req{conn=Conn, character=Character}=Req) ->
  ok = lmud_game:incarnate(Character),
  mn_conn:print(Conn, 'lmud-config':'post-login-msg'()),
  'msh-cmd-interact':glance([], Req),
  mn_conn:print(Conn, "\n> "),
  {ok, Req}.

