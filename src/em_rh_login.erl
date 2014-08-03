%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Login request handler.
%%% Will handle user login / new user creation, before passing the user on
%%% to the in-game request handler.
%%% @end
%%% =========================================================================
-module(em_rh_login).

%% API
-export([welcome/1, login/3]).

-include("request.hrl").

%% Type Specifications
-include("types.hrl").


%% ==========================================================================
%% API Functions
%% ==========================================================================

banner1() ->
"
MM\"\"\"\"\"\"\"\"`M          dP          M\"\"\"\"\"`'\"\"\"`YM M\"\"MMMMM\"\"M M\"\"\"\"\"\"'YMM
MM  mmmmmmmM          88          M  mm.  mm.  M M  MMMMM  M M  mmmm. `M
M`      MMMM 88d888b. 88 dP    dP M  MMM  MMM  M M  MMMMM  M M  MMMMM  M
MM  MMMMMMMM 88'  `88 88 88    88 M  MMM  MMM  M M  MMMMM  M M  MMMMM  M
MM  MMMMMMMM 88       88 88.  .88 M  MMM  MMM  M M  `MMM'  M M  MMMM' .M
MM        .M dP       dP `8888P88 M  MMM  MMM  M Mb       dM M       .MM
MMMMMMMMMMMM                  .88 MMMMMMMMMMMMMM MMMMMMMMMMM MMMMMMMMMMM
                          d8888P
".

banner2() ->
"
 _______       _       ______  _     _ _____
(_______)     | |     |  ___ \\| |   | (____ \\
 _____    ____| |_   _| | _ | | |   | |_   \\ \\
|  ___)  / ___) | | | | || || | |   | | |   | |
| |_____| |   | | |_| | || || | |___| | |__/ /
|_______)_|   |_|\\__  |_||_||_|\\______|_____/
                (____/
".

banner3() ->
"
   __     _                      ___
  /__\\ __| |_   _  /\\/\\  /\\ /\\  /   \\
 /_\\| '__| | | | |/    \\/ / \\ \\/ /\\ /
//__| |  | | |_| / /\\/\\ \\ \\_/ / /_//
\\__/|_|  |_|\\__, \\/    \\/\\___/___,'
            |___/

".

login_instructions() ->
"
*** If you are loging in for the first time, enter the character name
*** you would like to have (case insensitive) at the \"Login\" prompt.
".

post_login_msg () ->
"

You have logged into the server.

------------------------------------------------------------------------------
  \"WHO\" tells you who is logged in to the game.
  \"NEWS\" informs you about recent program changes and items of interest.
  \"HELP\" gives help on the commands, \"help commands\" for a list.
  \"QUIT\" saves your character exits the game.
------------------------------------------------------------------------------

".

-spec welcome(em_conn:conn_pid()) -> ok.
welcome(Conn) ->
  em_conn:print(Conn, "\nWelcome to:\n"
    ++ banner1() ++ "\nAn ErlyMUD Server, v"
    ++ em_util:get_version() ++ "\n\n"
    ++ login_instructions() ++ "\n\n"),
  em_conn:print(Conn, "Login: ").

%% Got a username, do something with it
-spec login(any(), string(), req()) -> req_ok() | req_link().
login(got_user, "", #req{conn=Conn}=Req) ->
  em_conn:print(Conn, "Invalid username.\n\n"),
  em_conn:print(Conn, "Login: "),
  ?req_next(login, [got_user]);
login(got_user, Name, #req{conn=Conn}=Req) ->
  UserName = em_text:capitalize(string:to_lower(Name)),
  UserFile = filename:join([em_game:data_dir(), "users",
                            [UserName, ".dat"]]),
  case file:consult(UserFile) of
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
  UserFile = filename:join([em_game:data_dir(), "users", [Name, ".dat"]]),
  CryptPw = base64:encode_to_string(em_util_sha2:hexdigest256(Password)),
  file:write_file(UserFile, lists:flatten([
    "{version, 1}.\n",
    "{password, \"", CryptPw, "\"}.\n"])),
  LivFile = filename:join([em_game:data_dir(), "livings", [Name, ".dat"]]),
  file:write_file(LivFile, lists:flatten([
    "{version, 1}.\n",
    "{long, \"", Name, " looks pretty ordinary.\"}.\n"])),
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
  {ok, User} = em_user_sup:start_child(Name, Conn),
  link(User),
  ok = em_user:load(User),
  case em_game:login(User) of
    ok ->
      {ok, Living} = em_living_sup:start_child(Name, {User, Conn}),
      link(Living),
      ok = em_living:load(Living),
      {ok, NewReq} = do_incarnate(Req#req{user=User, living=Living}),
      unlink(Living),
      unlink(User),
      ?req_next_and_link(em_rh_game, parse, [], NewReq);
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
  em_conn:print(Conn, post_login_msg()),
  em_rh_game:cmd_glance([], Req),
  em_conn:print(Conn, "\n> "),
  {ok, Req}.

