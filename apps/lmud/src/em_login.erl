%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Login request handler.
%%% Will handle user login / new user creation, before passing the user on
%%% to the in-game request handler.
%%% @end
%%% =========================================================================
-module(em_login).

%% API
-export([welcome/1, login/3]).

-include("apps/lmud/include/request.hrl").

%% Type Specifications
-include("apps/lmud/include/types.hrl").


%% ==========================================================================
%% API Functions
%% ==========================================================================

-spec welcome(em_conn:conn_pid()) -> ok.
welcome(Conn) ->
  em_conn:print(Conn, "\nWelcome to:\n"
    ++ 'lmud-config':banner() ++ "\nAn "
    ++ 'lmud-config':description() ++ ", v"
    ++ 'lmud-config':version() ++ "\n\n"
    ++ 'lmud-config':'login-instructions'() ++ "\n\n"),
  em_conn:print(Conn, "Login: ").

%% Got a username, do something with it
-spec login(any(), string(), req()) -> req_ok() | req_link().
login(got_user, "", #req{conn=Conn}=Req) ->
  em_conn:print(Conn, "Invalid username.\n\n"),
  em_conn:print(Conn, "Login: "),
  ?req_next(login, [got_user]);
login(got_user, UserName, #req{conn=Conn}=Req) ->
  case 'lmud-filestore':read("users", UserName) of
    {ok, Settings} ->
      em_conn:print(Conn, "Password: "),
      em_conn:echo_off(Conn),
      ?req_next(login, [{got_password, Settings, UserName}]);
    {error, _Reason} ->
      em_conn:print(Conn, "New user account \"~s\".\n", [UserName]),
      em_conn:print(Conn, "Are you sure (Y/N)? "),
      ?req_next(login, [{new_user_confirm, UserName}])
  end;
%% It's a new user, confirm if name is correct
login({new_user_confirm, Name}, YesNo, #req{conn=Conn}=Req) ->
  case string:to_lower(YesNo) of
    [$y|_Rest] ->
      em_conn:print(Conn, "\nPick a password: "),
      em_conn:echo_off(Conn),
      ?req_next(login, [{new_user_pw, Name}]);
    _Other ->
      em_conn:print(Conn, "\nLogin: "),
      ?req_next(login, [got_user])
  end;
%% Pick a password for new user account
login({new_user_pw, Name}, "", #req{conn=Conn}=Req) ->
  em_conn:print(Conn, "Invalid password.\n\n"),
  em_conn:print(Conn, "Pick a password: "),
  ?req_next(login, [{new_user_pw, Name}]);
login({new_user_pw, Name}, Password, #req{conn=Conn}=Req) ->
  em_conn:print(Conn, "\nRepeat the password: "),
  ?req_next(login, [{new_user_pw_confirm, Name, Password}]);
%% Confirm the password
login({new_user_pw_confirm, Name, Password}, Password, #req{conn=Conn}=Req) ->
  em_conn:echo_on(Conn),
  CryptPw = base64:encode_to_string(em_util_sha2:hexdigest256(Password)),
  'lmud-filestore':write("users", Name,
    lists:flatten([
    "{version, 1}.\n",
    "{password, \"", CryptPw, "\"}.\n"])),
  'lmud-filestore':write("livings", Name,
    lists:flatten([
    "{version, 1}.\n",
    "{desc, \"", Name, " looks pretty ordinary.\"}.\n"])),
  em_conn:print(Conn, ["\nWelcome, ", Name, "!\n\n"]),
  do_login(Name, Req);
login({new_user_pw_confirm, Name, _Password}, _WrongPassword, #req{conn=Conn}=Req) ->
  em_conn:print(Conn, "Passwords don't match, please try again.\n\n"),
  em_conn:print(Conn, "Pick a password: "),
  ?req_next(login, [{new_user_pw, Name}]);
%% Existing user; load file and compare passwords, log in if correct
login({got_password, Settings, Name}, Password, #req{conn=Conn}=Req) ->
  em_conn:echo_on(Conn),
  CryptPw = base64:encode_to_string(em_util_sha2:hexdigest256(Password)),
  case lists:keyfind(password, 1, Settings) of
    false ->
      em_conn:print(Conn, "Error: Invalid user file.\n\n"),
      em_conn:print(Conn, "Login: "),
      ?req_next(login, [got_user]);
    {password, CryptPw} ->
      do_login(Name, Req);
    {password, _Other} ->
      em_conn:print(Conn, "Invalid password.\n\n"),
      em_conn:print(Conn, "Login: "),
      ?req_next(login, [got_user])
  end.

-spec do_login(string(), req()) -> req_ok() | req_link().
do_login(Name, #req{conn=Conn}=Req) ->
  {ok, User} = 'lmud-user-sup':start_child(Name, Conn),
  link(User),
  ok = 'lmud-user':load(User),
  case em_game:login(User) of
    ok ->
      {ok, Living} = 'lmud-living-sup':start_child(Name, {User, Conn}),
      link(Living),
      ok = em_living:load(Living),
      {ok, NewReq} = do_incarnate(Req#req{user=User, living=Living}),
      unlink(Living),
      unlink(User),
      ?req_next_and_link(em_parser, parse, [], NewReq);
    {error, user_exists} ->
      unlink(User),
      exit(User, user_exists),
      em_conn:print(Conn, "User already logged in, try again.\n\n"),
      em_conn:print(Conn, "Login: "),
      ?req_next(login, [got_user])
  end.

-spec do_incarnate(req()) -> {ok, req()}.
do_incarnate(#req{conn=Conn, living=Living}=Req) ->
  ok = em_game:incarnate(Living),
  em_conn:print(Conn, 'lmud-config':'post-login-msg'()),
  'lmud-cmd-interact':glance([], Req),
  em_conn:print(Conn, "\n> "),
  {ok, Req}.

